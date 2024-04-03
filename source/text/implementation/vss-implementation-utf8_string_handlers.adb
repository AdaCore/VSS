--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic implementation of the string which use UTF-8 encoding for data.

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with Interfaces;

with VSS.Implementation.GCC;
with VSS.Implementation.Line_Iterators;

package body VSS.Implementation.UTF8_String_Handlers is

   use type VSS.Implementation.Strings.Character_Count;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;

   type Verification_State is
     (Initial,    --  ASCII or start of multibyte sequence
      U31,        --  A0 .. BF | UT1
      U33,        --  80 .. 9F | UT1
      U41,        --  90 .. BF | UT2
      U43,        --  80 .. 8F | UT2
      UT1,        --  1 (80 .. BF)
      UT2,        --  2 (80 .. BF)
      UT3,        --  3 (80 .. BF)
      Ill_Formed);
   --  Unicode defines well-formed UTF-8 as
   --
   --  00 .. 7F
   --  C2 .. DF | 80 .. BF
   --  E0       | A0 .. BF | 80 .. BF
   --  E1 .. EC | 80 .. BF | 80 .. BF
   --  ED       | 80 .. 9F | 80 .. BF
   --  EE .. EF | 80 .. BF | 80 .. BF
   --  F0       | 90 .. BF | 80 .. BF | 80 .. BF
   --  F1 .. F3 | 80 .. BF | 80 .. BF | 80 .. BF
   --  F4       | 80 .. 8F | 80 .. BF | 80 .. BF

   procedure Unchecked_Forward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Move cursor to position of the next character

   procedure Unchecked_Backward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Move cursor to position of the previous character

   procedure Validate_And_Copy
     (Source      : Ada.Strings.UTF_Encoding.UTF_8_String;
      Destination : out VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length      : out VSS.Implementation.Strings.Character_Count;
      Success     : out Boolean);
   --  Validate UTF-8 encoding and copy validated part of the data to
   --  Destination. Length is set to the length of the text in characters.
   --  Success is set False when validation is failed and to True otherwise.

   Growth_Factor            : constant := 32;
   --  The growth factor controls how much extra space is allocated when
   --  we have to increase the size of an allocated unbounded string. By
   --  allocating extra space, we avoid the need to reallocate on every
   --  append, particularly important when a string is built up by repeated
   --  append operations of small pieces. This is expressed as a factor so
   --  32 means add 1/32 of the length of the string as growth space.

   Minimal_Allocation_Block : constant := Standard'Maximum_Alignment;
   --  Allocation will be done by a multiple of Minimal_Allocation_Block.
   --  This causes no memory loss as most (all?) malloc implementations are
   --  obliged to align the returned memory on the maximum alignment as malloc
   --  does not know the target alignment.

   function Aligned_Capacity
     (Capacity : VSS.Unicode.UTF8_Code_Unit_Count)
      return VSS.Unicode.UTF8_Code_Unit_Count;
   --  Returns recommended capacity of the string data storage which is greater
   --  or equal to the specified requested capacity. Calculation take in sense
   --  alignment of the allocated memory segments to use memory effectively by
   --  sequential operations which extends size of the buffer.

   procedure Unreference (Data : in out UTF8_String_Data_Access);
   --  Decrement reference counter and deallocate storage block when reference
   --  counter reach zero.

   procedure Mutate
     (Data     : in out UTF8_String_Data_Access;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Reallocate storage block when it is shared or not enough to store given
   --  amount of data.

   procedure Split_Lines_Common
     (Handler         :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Data            : VSS.Implementation.Strings.String_Data;
      Storage         : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);
   --  Common code of Split_Lines subprogram for on heap and inline handlers.

   procedure Internal_Append
     (Storage        : in out
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length         : in out VSS.Implementation.Strings.Character_Count;
      Size           : in out VSS.Unicode.UTF8_Code_Unit_Count;
      Suffix_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Suffix_Length  : VSS.Implementation.Strings.Character_Count;
      Suffix_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Offset         : in out VSS.Implementation.Strings.Cursor_Offset)
     with Pre => Storage'Last >= Size + Suffix_Size;
   --  Append one storage to another.

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position :=
        (Index        => Self.Pointer.Length + 1,
         UTF8_Offset  => Self.Pointer.Size,
         UTF16_Offset => 0);
   end After_Last_Character;

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position :=
        (Index        => Self.Length + 1,
         UTF8_Offset  => Self.Size,
         UTF16_Offset => 0);
   end After_Last_Character;

   ----------------------
   -- Aligned_Capacity --
   ----------------------

   function Aligned_Capacity
     (Capacity : VSS.Unicode.UTF8_Code_Unit_Count)
      return VSS.Unicode.UTF8_Code_Unit_Count
   is
      subtype Empty_UTF8_String_Data is UTF8_String_Data (0);

      Static_Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        Empty_UTF8_String_Data'Max_Size_In_Storage_Elements;

   begin
      return
        ((Static_Size + Capacity) / Minimal_Allocation_Block + 1)
          * Minimal_Allocation_Block - Static_Size;
   end Aligned_Capacity;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count)
      return UTF8_String_Data_Access is
   begin
      return
        new UTF8_String_Data
              (Aligned_Capacity
                 (VSS.Unicode.UTF8_Code_Unit_Count'Max (Capacity, Size)));
   end Allocate;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out UTF8_String_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      L  : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
      U1 : VSS.Unicode.UTF8_Code_Unit;
      U2 : VSS.Unicode.UTF8_Code_Unit;
      U3 : VSS.Unicode.UTF8_Code_Unit;
      U4 : VSS.Unicode.UTF8_Code_Unit;

   begin
      VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

      Offset.Index_Offset := Offset.Index_Offset + 1;
      Offset.UTF8_Offset  := Offset.UTF8_Offset + L;
      Offset.UTF16_Offset := Offset.UTF16_Offset + (if L = 4 then 2 else 1);

      Mutate
        (Self.Pointer,
         VSS.Unicode.UTF8_Code_Unit_Count (Self.Unsafe_Capacity) * 4,
         Self.Pointer.Size + L);

      VSS.Implementation.UTF8_Encoding.Unchecked_Store
        (Self.Pointer.Storage, Self.Pointer.Size, L, U1, U2, U3, U4);

      Self.Pointer.Size := Self.Pointer.Size + L;
      Self.Pointer.Length := Self.Pointer.Length + 1;
      Self.Pointer.Storage (Self.Pointer.Size) := 16#00#;
   end Append;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out UTF8_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Parent : VSS.Implementation.Text_Handlers.Abstract_String_Handler
        renames VSS.Implementation.Text_Handlers.Abstract_String_Handler
                  (Self);

      Suffix_Handler : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Suffix);

   begin
      if Suffix_Handler.all in UTF8_In_Place_String_Handler then
         --  Suffix is static storage.

         declare
            Suffix_Static : UTF8_In_Place_String_Handler
              renames UTF8_In_Place_String_Handler (Suffix_Handler.all);

         begin
            Mutate
              (Self.Pointer,
               VSS.Unicode.UTF8_Code_Unit_Count (Data.Capacity) * 4,
               Self.Pointer.Size + Suffix_Static.Size);

            Internal_Append
              (Storage        => Self.Pointer.Storage,
               Length         => Self.Pointer.Length,
               Size           => Self.Pointer.Size,
               Suffix_Storage => Suffix_Static.Storage,
               Suffix_Length  => Suffix_Static.Length,
               Suffix_Size    => Suffix_Static.Size,
               Offset         => Offset);
         end;

      elsif Suffix_Handler.all in UTF8_String_Handler then
         --  Suffix is dynamic storage.

         declare
            Suffix_Dynamic : UTF8_String_Handler
              renames UTF8_String_Handler (Suffix_Handler.all);

         begin
            Mutate
              (Self.Pointer,
               VSS.Unicode.UTF8_Code_Unit_Count (Data.Capacity) * 4,
               Self.Pointer.Size + Suffix_Dynamic.Pointer.Size);

            Internal_Append
              (Storage        => Self.Pointer.Storage,
               Length         => Self.Pointer.Length,
               Size           => Self.Pointer.Size,
               Suffix_Storage => Suffix_Dynamic.Pointer.Storage,
               Suffix_Length  => Suffix_Dynamic.Pointer.Length,
               Suffix_Size    => Suffix_Dynamic.Pointer.Size,
               Offset         => Offset);
         end;

      else
         --  Suffix is not an UTF-8 text, no other optimization is possible
         --  here, copy character by character.

         Parent.Append (Data, Suffix, Offset);
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out UTF8_In_Place_String_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      L  : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
      U1 : VSS.Unicode.UTF8_Code_Unit;
      U2 : VSS.Unicode.UTF8_Code_Unit;
      U3 : VSS.Unicode.UTF8_Code_Unit;
      U4 : VSS.Unicode.UTF8_Code_Unit;

   begin
      VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

      Offset.Index_Offset := Offset.Index_Offset + 1;
      Offset.UTF8_Offset  := Offset.UTF8_Offset + L;
      Offset.UTF16_Offset := Offset.UTF16_Offset + (if L = 4 then 2 else 1);

      if Self.Storage'Last >= Self.Size + L then
         --  There is enough space to store data in place.

         VSS.Implementation.UTF8_Encoding.Unchecked_Store
           (Self.Storage, Self.Size, L, U1, U2, U3, U4);

         Self.Size   := @ + L;
         Self.Length := @ + 1;
         Self.Storage (Self.Size) := 16#00#;

      else
         --  Data can't be stored in the static storage and need to be
         --  converted into dynamic storage.
         Convert_To_Dynamic
           (Self,
            VSS.Unicode.UTF8_Code_Unit_Count (Self.Unsafe_Capacity * 4),
            Self.Size + L);

         declare
            Dynamic : UTF8_String_Handler
              renames UTF8_String_Handler
              (VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class
                 (Self));

         begin
            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Dynamic.Pointer.Storage,
               Dynamic.Pointer.Size, L, U1, U2, U3, U4);

            Dynamic.Pointer.Size   := @ + L;
            Dynamic.Pointer.Length := @ + 1;
            Dynamic.Pointer.Storage (Dynamic.Pointer.Size) := 16#00#;
         end;
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out UTF8_In_Place_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Parent : VSS.Implementation.Text_Handlers.Abstract_String_Handler
        renames VSS.Implementation.Text_Handlers.Abstract_String_Handler
                  (Self);

      Suffix_Handler : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Suffix);

   begin
      if Suffix_Handler.all in UTF8_String_Handler then
         --  The suffix use dynamic storage. However, in some cases, result
         --  might be stored in the current static storage.

         declare
            Suffix_Dynamic : UTF8_String_Handler
              renames UTF8_String_Handler (Suffix_Handler.all);
            Handler        :
              VSS.Implementation.Strings.Variable_Text_Handler_Access;

         begin
            if Self.Storage'Last
                 >= Self.Size + Suffix_Dynamic.Pointer.Size
            then
               Internal_Append
                 (Storage        => Self.Storage,
                  Length         => Self.Length,
                  Size           => Self.Size,
                  Suffix_Storage => Suffix_Dynamic.Pointer.Storage,
                  Suffix_Length  => Suffix_Dynamic.Pointer.Length,
                  Suffix_Size    => Suffix_Dynamic.Pointer.Size,
                  Offset         => Offset);

            else
               --  Size of the current static storge is not enough, move
               --  current text into dynamic storage, and call handler of
               --  the dynamic storage to append text.

               Convert_To_Dynamic
                 (Self,
                  VSS.Unicode.UTF8_Code_Unit_Count (Data.Capacity * 4),
                  Self.Size + Suffix_Dynamic.Pointer.Size);

               Handler := VSS.Implementation.Strings.Variable_Handler (Data);
               Handler.Append (Data, Suffix, Offset);
            end if;
         end;

      elsif Suffix_Handler.all in UTF8_In_Place_String_Handler then
         declare
            Suffix_Static : UTF8_In_Place_String_Handler
              renames UTF8_In_Place_String_Handler (Suffix_Handler.all);
            Handler       :
              VSS.Implementation.Strings.Variable_Text_Handler_Access;

         begin
            if Self.Storage'Last >= Self.Size + Suffix_Static.Size then
               --  There is enough space to store data in place.

               Internal_Append
                 (Storage        => Self.Storage,
                  Length         => Self.Length,
                  Size           => Self.Size,
                  Suffix_Storage => Suffix_Static.Storage,
                  Suffix_Length  => Suffix_Static.Length,
                  Suffix_Size    => Suffix_Static.Size,
                  Offset         => Offset);

            else
               --  Data can't be stored in static text storage and need to be
               --  converted into a dynamic text storage.

               Convert_To_Dynamic
                 (Self,
                  VSS.Unicode.UTF8_Code_Unit_Count (Data.Capacity * 4),
                  Self.Size + Suffix_Static.Size);

               Handler := VSS.Implementation.Strings.Variable_Handler (Data);
               Handler.Append (Data, Suffix, Offset);
            end if;
         end;

      else
         --  Can't be optimize, use char-by-char append

         Parent.Append (Data, Suffix, Offset);
      end if;
   end Append;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      if Position.Index = 0 then
         return False;

      else
         Unchecked_Backward (Self.Pointer.Storage, Position);
      end if;

      return Position.Index > 0;
   end Backward;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      if Position.Index = 0 then
         return False;

      else
         Unchecked_Backward (Self.Storage, Position);
      end if;

      return Position.Index > 0;
   end Backward;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   overriding procedure Before_First_Character
     (Self     : UTF8_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position := (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   end Before_First_Character;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   overriding procedure Before_First_Character
     (Self     : UTF8_In_Place_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position := (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   end Before_First_Character;

   ------------------------
   -- Convert_To_Dynamic --
   ------------------------

   procedure Convert_To_Dynamic
     (Text     : in out UTF8_In_Place_String_Handler;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      Pointer : constant UTF8_String_Data_Access := Allocate (Capacity, Size);

   begin
      Pointer.Storage (0 .. Text.Size) := Text.Storage (0 .. Text.Size);
      Pointer.Length                   := Text.Length;
      Pointer.Size                     := Text.Size;

      declare
         Overlay : UTF8_String_Handler := (others => <>)
           with Address => Text'Address;

      begin
         Overlay := (Pointer => Pointer);
      end;
   end Convert_To_Dynamic;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Offset :=
        Self.Pointer.Size - Size.UTF8_Offset;

   begin
      if Size.Index_Offset = 0 then
         return;
      end if;

      Self.Pointer.Storage (From.UTF8_Offset .. New_Size) :=
        Self.Pointer.Storage
          (From.UTF8_Offset + Size.UTF8_Offset .. Self.Pointer.Size);

      Self.Pointer.Length := @ - Size.Index_Offset;
      Self.Pointer.Size   := New_Size;
   end Delete;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out UTF8_In_Place_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Offset :=
        Self.Size - Size.UTF8_Offset;

   begin
      if Size.Index_Offset = 0 then
         return;
      end if;

      Self.Storage (From.UTF8_Offset .. New_Size) :=
        Self.Storage (From.UTF8_Offset + Size.UTF8_Offset .. Self.Size);

      Self.Length := @ - Size.Index_Offset;
      Self.Size   := New_Size;
   end Delete;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base is
   begin
      if Position.Index < 1
        or else Position.Index > Self.Pointer.Length
      then
         return VSS.Implementation.Strings.No_Character;
      end if;

      return
        VSS.Implementation.UTF8_Encoding.Unchecked_Decode
          (Self.Pointer.Storage, Position.UTF8_Offset);
   end Element;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base is
   begin
      if Position.Index < 1
        or else Position.Index > Self.Length
      then
         return VSS.Implementation.Strings.No_Character;
      end if;

      return
        VSS.Implementation.UTF8_Encoding.Unchecked_Decode
          (Self.Storage, Position.UTF8_Offset);
   end Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : UTF8_String_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean
   is
      pragma Suppress (Access_Check);

   begin
      if Position.Index > Self.Pointer.Length then
         return False;

      else
         Unchecked_Forward (Self.Pointer.Storage, Position);
      end if;

      return Position.Index <= Self.Pointer.Length;
   end Forward;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : UTF8_In_Place_String_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean is
   begin
      if Position.Index > Self.Length then
         return False;

      else
         Unchecked_Forward (Self.Storage, Position);
      end if;

      return Position.Index <= Self.Length;
   end Forward;

   ---------------------
   -- Forward_Element --
   ---------------------

   overriding function Forward_Element
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean
   is
      Code   : VSS.Unicode.Code_Point'Base :=
        VSS.Implementation.Strings.No_Character;
      Result : Boolean := False;

   begin
      if Position.Index <= Self.Pointer.Length then
         Unchecked_Forward (Self.Pointer.Storage, Position);

         if Position.Index <= Self.Pointer.Length then
            Code :=
              VSS.Implementation.UTF8_Encoding.Unchecked_Decode
                (Self.Pointer.Storage, Position.UTF8_Offset);
            Result := True;
         end if;
      end if;

      Element := Code;

      return Result;
   end Forward_Element;

   ---------------------
   -- Forward_Element --
   ---------------------

   overriding function Forward_Element
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean
   is
      Code   : VSS.Unicode.Code_Point'Base :=
        VSS.Implementation.Strings.No_Character;
      Result : Boolean := False;

   begin
      if Position.Index <= Self.Length then
         Unchecked_Forward (Self.Storage, Position);

         if Position.Index <= Self.Length then
            Code :=
              VSS.Implementation.UTF8_Encoding.Unchecked_Decode
                (Self.Storage, Position.UTF8_Offset);
            Result := True;
         end if;
      end if;

      Element := Code;

      return Result;
   end Forward_Element;

   -----------------------
   -- From_UTF_8_String --
   -----------------------

   overriding procedure From_UTF_8_String
     (Self    : in out UTF8_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean)
   is
      Pointer : UTF8_String_Data_Access;
      Length  : VSS.Implementation.Strings.Character_Count := 0;

   begin
      Pointer := Allocate (0, Item'Length);

      Validate_And_Copy (Item, Pointer.Storage, Length, Success);

      if Success then
         Pointer.Storage
           (VSS.Unicode.UTF8_Code_Unit_Count (Item'Length)) := 16#00#;
         --  GNAT 20240327: compiler crash without type conversion.
         Pointer.Length := Length;
         Pointer.Size   := Item'Length;
         Self.Pointer   := Pointer;

      else
         Unreference (Pointer);
      end if;
   end From_UTF_8_String;

   -----------------------
   -- From_UTF_8_String --
   -----------------------

   overriding procedure From_UTF_8_String
     (Self    : in out UTF8_In_Place_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean)
   is
      Length : VSS.Implementation.Strings.Character_Count;

   begin
      if Item'Length >= Self.Storage'Length then
         --  There is not enough space to store data

         Success := False;

         return;
      end if;

      Validate_And_Copy (Item, Self.Storage, Length, Success);

      if Success then
         Self.Storage (Item'Length) := 16#00#;
         Self.Length := Length;
         Self.Size   := Item'Length;
      end if;
   end From_UTF_8_String;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   overriding procedure From_Wide_Wide_String
     (Self    : in out UTF8_String_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean) is
   begin
      Success := True;

      declare
         Pointer : UTF8_String_Data_Access;
         Code    : VSS.Unicode.Code_Point;
         L       : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
         U1      : VSS.Unicode.UTF8_Code_Unit;
         U2      : VSS.Unicode.UTF8_Code_Unit;
         U3      : VSS.Unicode.UTF8_Code_Unit;
         U4      : VSS.Unicode.UTF8_Code_Unit;

      begin
         Pointer := Allocate (0, Item'Length);

         for C of Item loop
            if Wide_Wide_Character'Pos (C) not in VSS.Unicode.Code_Point
              or else Wide_Wide_Character'Pos (C) in 16#D800# .. 16#DFFF#
            then
               Success := False;

               exit;

            else
               Code := Wide_Wide_Character'Pos (C);
            end if;

            VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

            if Pointer.Bulk < Pointer.Size + L then
               --  There is no enough storage to store character, reallocate
               --  memory.

               Reallocate (Pointer, 0, Pointer.Size + L);
            end if;

            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Pointer.Storage, Pointer.Size, L, U1, U2, U3, U4);

            Pointer.Size := Pointer.Size + L;
         end loop;

         if Success then
            Pointer.Storage (Pointer.Size) := 16#00#;
            Pointer.Length := Item'Length;
            Self.Pointer   := Pointer;

         else
            Unreference (Pointer);
         end if;
      end;
   end From_Wide_Wide_String;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   overriding procedure From_Wide_Wide_String
     (Self    : in out UTF8_In_Place_String_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean) is
   begin
      Success := True;

      declare
         Code : VSS.Unicode.Code_Point;
         L    : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
         U1   : VSS.Unicode.UTF8_Code_Unit;
         U2   : VSS.Unicode.UTF8_Code_Unit;
         U3   : VSS.Unicode.UTF8_Code_Unit;
         U4   : VSS.Unicode.UTF8_Code_Unit;

      begin
         Self.Size := 0;

         for C of Item loop
            if Wide_Wide_Character'Pos (C) not in VSS.Unicode.Code_Point
              or else Wide_Wide_Character'Pos (C) in 16#D800# .. 16#DFFF#
            then
               Success := False;

               exit;

            else
               Code := Wide_Wide_Character'Pos (C);
            end if;

            VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

            if Self.Storage'Last < Self.Size + L then
               Unsafe_Initialize (Self, 0, Self.Size + L);

               VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class
                 (Self).From_Wide_Wide_String (Item, Success);

               return;
            end if;

            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Self.Storage, Self.Size, L, U1, U2, U3, U4);

            Self.Size := @ + L;
         end loop;

         if Success then
            Self.Length := Item'Length;
            Self.Storage (Self.Size) := 16#00#;
         end if;
      end;
   end From_Wide_Wide_String;

   -------------------
   -- Has_Character --
   -------------------

   overriding function Has_Character
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      return Position.Index in 1 .. Self.Pointer.Length;
   end Has_Character;

   -------------------
   -- Has_Character --
   -------------------

   overriding function Has_Character
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      return Position.Index in 1 .. Self.Length;
   end Has_Character;

   ----------------
   -- Initialize --
   ----------------

   --  overriding procedure Initialize
   --    (Self : UTF8_String_Handler;
   --     Data : out VSS.Implementation.Strings.String_Data) is
   --  begin
   --     Self.Pointer := Allocate (0, 0);
   --  end Initialize;

   ----------------
   -- Initialize --
   ----------------

   --  overriding procedure Initialize
   --    (Self : UTF8_In_Place_String_Handler;
   --     Data : out VSS.Implementation.Strings.String_Data) is
   --  begin
   --     Data :=
   --       (In_Place => True,
   --        Capacity => 0,
   --        Storage  => <>,
   --        --  Handler  => Self'Unrestricted_Access,
   --        --  Pointer  => SYstem.Null_Address,
   --        Padding  => False);
   --
   --     declare
   --        Target : UTF8_In_Place_Data
   --          with Import, Convention => Ada, Address => Data'Address;
   --
   --     begin
   --        Target := (Storage => <>, Size => 0, Length => 0);
   --     end;
   --  end Initialize;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out UTF8_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      L  : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
      U1 : VSS.Unicode.UTF8_Code_Unit;
      U2 : VSS.Unicode.UTF8_Code_Unit;
      U3 : VSS.Unicode.UTF8_Code_Unit;
      U4 : VSS.Unicode.UTF8_Code_Unit;

   begin
      if VSS.Implementation.Strings.Is_Invalid (From) then
         return;
      end if;

      VSS.Implementation.UTF8_Encoding.Encode (Item, L, U1, U2, U3, U4);

      Offset.Index_Offset := Offset.Index_Offset + 1;
      Offset.UTF8_Offset  := Offset.UTF8_Offset + L;
      Offset.UTF16_Offset := Offset.UTF16_Offset + (if L = 4 then 2 else 1);

      Mutate
        (Self.Pointer,
         VSS.Unicode.UTF8_Code_Unit_Count (Data.Capacity) * 4,
         Self.Pointer.Size + L);

      Self.Pointer.Storage
        (From.UTF8_Offset + L .. Self.Pointer.Size + L) :=
           Self.Pointer.Storage (From.UTF8_Offset .. Self.Pointer.Size);

      VSS.Implementation.UTF8_Encoding.Unchecked_Store
        (Self.Pointer.Storage, From.UTF8_Offset, L, U1, U2, U3, U4);

      Self.Pointer.Size   := @ + L;
      Self.Pointer.Length := @ + 1;
      Self.Pointer.Storage (Self.Pointer.Size) := 16#00#;
   end Insert;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out UTF8_In_Place_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      L           : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
      U1          : VSS.Unicode.UTF8_Code_Unit;
      U2          : VSS.Unicode.UTF8_Code_Unit;
      U3          : VSS.Unicode.UTF8_Code_Unit;
      U4          : VSS.Unicode.UTF8_Code_Unit;

   begin
      if VSS.Implementation.Strings.Is_Invalid (From) then
         return;
      end if;

      VSS.Implementation.UTF8_Encoding.Encode (Item, L, U1, U2, U3, U4);

      Offset.Index_Offset := Offset.Index_Offset + 1;
      Offset.UTF8_Offset  := Offset.UTF8_Offset + L;
      Offset.UTF16_Offset := Offset.UTF16_Offset + (if L = 4 then 2 else 1);

      if Self.Storage'Last >= Self.Size + L then
         --  There is enough space to store data in place.

         Self.Storage
           (From.UTF8_Offset + L .. Self.Size + L) :=
              Self.Storage (From.UTF8_Offset .. Self.Size);

         VSS.Implementation.UTF8_Encoding.Unchecked_Store
           (Self.Storage, From.UTF8_Offset, L, U1, U2, U3, U4);

         Self.Size   := @ + L;
         Self.Length := @ + 1;
         Self.Storage (Self.Size) := 16#00#;

      else
         --  Data can't be stored "in place" and need to be converted into
         --  shared data.

         Convert_To_Dynamic
           (Self,
            VSS.Unicode.UTF8_Code_Unit_Count (Data.Capacity * 4),
            Self.Size + L);

         declare
            Handler : UTF8_String_Handler
              renames UTF8_String_Handler
                (VSS.Implementation.Strings.Variable_Handler (Data).all);

         begin
            Handler.Pointer.Storage
              (From.UTF8_Offset + L .. Handler.Pointer.Size + L) :=
                 Handler.Pointer.Storage
                   (From.UTF8_Offset .. Handler.Pointer.Size);

            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Handler.Pointer.Storage, From.UTF8_Offset, L, U1, U2, U3, U4);

            Handler.Pointer.Size   := @ + L;
            Handler.Pointer.Length := @ + 1;
            Handler.Pointer.Storage (Handler.Pointer.Size) := 16#00#;
         end;
      end if;
   end Insert;

   ---------------------
   -- Internal_Append --
   ---------------------

   procedure Internal_Append
     (Storage        : in out
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length         : in out VSS.Implementation.Strings.Character_Count;
      Size           : in out VSS.Unicode.UTF8_Code_Unit_Count;
      Suffix_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Suffix_Length  : VSS.Implementation.Strings.Character_Count;
      Suffix_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Offset         : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      use type VSS.Unicode.UTF8_Code_Unit;

      Suffix_UTF16_Size : VSS.Unicode.UTF16_Code_Unit_Count :=
        VSS.Unicode.UTF16_Code_Unit_Count (Suffix_Length);
      --  Size of the suffix in UTF-16 code units. This is first approximation
      --  only, it is corrected later.

   begin
      for J in 0 .. Suffix_Size loop
         Storage (Size + J) := Suffix_Storage (J);

         if (Suffix_Storage (J) and 2#1111_1000#) = 2#1111_0000# then
            Suffix_UTF16_Size := Suffix_UTF16_Size + 1;
            --  Encoded character occupy two UTF-16 code units.
         end if;
      end loop;

      Size   := @ + Suffix_Size;
      Length := @ + Suffix_Length;

      Offset.Index_Offset := @ + Suffix_Length;
      Offset.UTF8_Offset  := @ + Suffix_Size;
      Offset.UTF16_Offset := @ + Suffix_UTF16_Size;

      Storage (Size) := 16#00#;
   end Internal_Append;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Self : UTF8_String_Handler;
      Data : VSS.Implementation.Strings.String_Data) return Boolean is
   begin
      return Self.Pointer.Length = 0;
   end Is_Empty;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Self : UTF8_In_Place_String_Handler;
      Data : VSS.Implementation.Strings.String_Data) return Boolean is
   begin
      return Self.Length = 0;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : UTF8_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return VSS.Implementation.Strings.Character_Count is
   begin
      return Self.Pointer.Length;
   end Length;

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : UTF8_In_Place_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return VSS.Implementation.Strings.Character_Count is
   begin
      return Self.Length;
   end Length;

   ------------
   -- Mutate --
   ------------

   procedure Mutate
     (Data     : in out UTF8_String_Data_Access;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      if Data = null then
         Data := Allocate (Capacity, Size);

      elsif not System.Atomic_Counters.Is_One (Data.Counter)
        or else Size > Data.Bulk
      then
         Reallocate (Data, Capacity, Size);
      end if;
   end Mutate;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate
     (Data     : in out UTF8_String_Data_Access;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      Minimal_Capacity : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        (if Capacity >= Size
         then Capacity
         else (if Data = null
               then Size
               else (if Data.Bulk > Size
                     then Size
                     else Size + Size / Growth_Factor)));

      Aux : UTF8_String_Data_Access := Data;

   begin
      Data := Allocate (0, Minimal_Capacity);

      if Aux /= null then
         declare
            Last : constant VSS.Unicode.UTF8_Code_Unit_Count :=
              VSS.Unicode.UTF8_Code_Unit_Count'Min (Data.Bulk, Aux.Bulk);

         begin
            Data.Storage (0 .. Last) := Aux.Storage (0 .. Last);
            Data.Size := Aux.Size;
            Data.Length := Aux.Length;
            Unreference (Aux);
         end;
      end if;
   end Reallocate;

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference
     (Self : in out UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is
   begin
      --  if Self.Pointer /= null then
      System.Atomic_Counters.Increment (Self.Pointer.Counter);
      --  end if;
   end Reference;

   -----------
   -- Slice --
   -----------

   overriding procedure Slice
     (Self        : UTF8_String_Handler;
      Source_Data : VSS.Implementation.Strings.String_Data;
      From        : VSS.Implementation.Strings.Cursor;
      To          : VSS.Implementation.Strings.Cursor;
      Target_Data : out VSS.Implementation.Strings.String_Data)
   is
      Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Length : VSS.Implementation.Strings.Character_Count;
      After  : VSS.Implementation.Strings.Cursor := To;

   begin
      if From.Index > To.Index then
         Target_Data := VSS.Implementation.Strings.Null_String_Data;

         return;
      end if;

      Unchecked_Forward (Self.Pointer.Storage, After);

      Size   := After.UTF8_Offset - From.UTF8_Offset;
      Length := After.Index - From.Index;

      if Size <= In_Place_Storage_Capacity then
         declare
            Static : UTF8_In_Place_String_Handler
              with Import,
                   Convention => Ada,
                   Address    => Target_Data.Storage'Address;

         begin
            Static := (others => <>);

            Static.Storage (0 .. Size - 1) :=
              Self.Pointer.Storage (From.UTF8_Offset .. After.UTF8_Offset - 1);
            Static.Size   := Size;
            Static.Length := Length;
            Static.Storage (Static.Size) := 16#00#;
         end;

      else
         declare
            Pointer : UTF8_String_Data_Access;
            Dynamic : UTF8_String_Handler
              with Import,
                   Convention => Ada,
                   Address    => Target_Data.Storage'Address;

         begin
            Dynamic := (others => <>);

            Pointer := Allocate (0, Size);
            Pointer.Storage (0 .. Size - 1) :=
              Self.Pointer.Storage (From.UTF8_Offset .. After.UTF8_Offset - 1);
            Pointer.Size   := Size;
            Pointer.Length := Length;
            Pointer.Storage (Pointer.Size) := 16#00#;

            Dynamic.Pointer := Pointer;
         end;
      end if;
   end Slice;

   -----------
   -- Slice --
   -----------

   overriding procedure Slice
     (Self        : UTF8_In_Place_String_Handler;
      Source_Data : VSS.Implementation.Strings.String_Data;
      From        : VSS.Implementation.Strings.Cursor;
      To          : VSS.Implementation.Strings.Cursor;
      Target_Data : out VSS.Implementation.Strings.String_Data)
   is
      Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Length : VSS.Implementation.Strings.Character_Count;
      After  : VSS.Implementation.Strings.Cursor := To;

   begin
      if From.Index > To.Index then
         Target_Data := VSS.Implementation.Strings.Null_String_Data;

         return;
      end if;

      Unchecked_Forward (Self.Storage, After);

      Size   := After.UTF8_Offset - From.UTF8_Offset;
      Length := After.Index - From.Index;

      declare
         Static : UTF8_In_Place_String_Handler
           with Import,
                Convention => Ada,
                Address    => Target_Data.Storage'Address;

      begin
         Static := (others => <>);

         Static.Storage (0 .. Size - 1) :=
           Self.Storage (From.UTF8_Offset .. After.UTF8_Offset - 1);
         Static.Size   := Size;
         Static.Length := Length;
         Static.Storage (Static.Size) := 16#00#;
      end;
   end Slice;

   -----------------
   -- Split_Lines --
   -----------------

   overriding procedure Split_Lines
     (Self            : UTF8_String_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access) is
   begin
      Split_Lines_Common
        (Handler         => Self,
         Data            => Data,
         Storage         => Self.Pointer.Storage,
         Terminators     => Terminators,
         Keep_Terminator => Keep_Terminator,
         Lines           => Lines);
   end Split_Lines;

   -----------------
   -- Split_Lines --
   -----------------

   overriding procedure Split_Lines
     (Self            : UTF8_In_Place_String_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access) is
   begin
      Split_Lines_Common
        (Handler         => Self,
         Data            => Data,
         Storage         => Self.Storage,
         Terminators     => Terminators,
         Keep_Terminator => Keep_Terminator,
         Lines           => Lines);
   end Split_Lines;

   ------------------------
   -- Split_Lines_Common --
   ------------------------

   procedure Split_Lines_Common
     (Handler         :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Data            : VSS.Implementation.Strings.String_Data;
      Storage         : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
   is
      procedure Append
        (Source_Storage :
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         First          : VSS.Implementation.Strings.Cursor;
         After_Last     : VSS.Implementation.Strings.Cursor);

      ------------
      -- Append --
      ------------

      procedure Append
        (Source_Storage :
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         First          : VSS.Implementation.Strings.Cursor;
         After_Last     : VSS.Implementation.Strings.Cursor)
      is
         Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
           After_Last.UTF8_Offset - First.UTF8_Offset;
         Data : VSS.Implementation.Strings.String_Data;

      begin
         if Size = 0 then
            null;

         elsif Size <= In_Place_Storage_Capacity then
            --  Static storage can be used.

            declare
               Static : UTF8_In_Place_String_Handler
                 with Import,
                      Convention => Ada,
                      Address    => Data.Storage'Address;

            begin
               Static.Storage (0 .. Size - 1) :=
                 Source_Storage
                   (First.UTF8_Offset .. After_Last.UTF8_Offset - 1);
               Static.Storage (Size) := 0;
               Static.Size   := Size;
               Static.Length := After_Last.Index - First.Index;
            end;

         else
            declare
               Pointer : UTF8_String_Data_Access;
               Dynamic : UTF8_String_Handler
                 with Import,
                      Convention => Ada,
                      Address    => Data.Storage'Address;

            begin
               Pointer := Allocate (0, Size);

               Pointer.Storage (0 .. Size - 1) :=
                 Source_Storage
                   (First.UTF8_Offset .. After_Last.UTF8_Offset - 1);
               Pointer.Size   := Size;
               Pointer.Length := After_Last.Index - First.Index;
               Pointer.Storage (Pointer.Size) := 16#00#;

               Dynamic.Pointer := Pointer;
            end;
         end if;

         VSS.Implementation.String_Vectors.Append_And_Move_Ownership
           (Lines, Data);
      end Append;

      Initial    : VSS.Implementation.Strings.Cursor;
      At_First   : aliased VSS.Implementation.Strings.Cursor;
      At_Last    : aliased VSS.Implementation.Strings.Cursor;
      After_Last : aliased VSS.Implementation.Strings.Cursor;
      Terminator : VSS.Implementation.Strings.Cursor;
      Dummy      : Boolean;

   begin
      VSS.Implementation.String_Vectors.Unreference (Lines);

      Handler.Before_First_Character (Initial);

      while VSS.Implementation.Line_Iterators.Forward
        (Data,
         Terminators,
         Initial,
         At_First,
         At_Last,
         Terminator)
      loop
         Initial := At_Last;

         if VSS.Implementation.Strings.Is_Invalid (Terminator) then
            After_Last := At_Last;
            Dummy      := Handler.Forward (After_Last);

         elsif Keep_Terminator then
            After_Last := At_Last;
            Dummy      := Handler.Forward (After_Last);

         else
            After_Last := Terminator;
         end if;

         Append (Storage, At_First, After_Last);
      end loop;
   end Split_Lines_Common;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : UTF8_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return Result : Ada.Strings.UTF_Encoding.UTF_8_String
                        (1 .. Natural (Self.Pointer.Size))
      do
         for J in Result'Range loop
            Result (J) :=
              Standard.Character'Val
                (Self.Pointer.Storage
                   (VSS.Unicode.UTF8_Code_Unit_Count (J - 1)));
         end loop;
      end return;
   end To_UTF_8_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : UTF8_In_Place_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return Result : Ada.Strings.UTF_Encoding.UTF_8_String
                        (1 .. Natural (Self.Size))
      do
         for J in Result'Range loop
            Result (J) :=
              Standard.Character'Val
                (Self.Storage (VSS.Unicode.UTF8_Code_Unit_Count (J - 1)));
         end loop;
      end return;
   end To_UTF_8_String;

   ----------------------
   -- Unchecked_Append --
   ----------------------

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False)
   is
      Target_Handler : constant not null
        VSS.Implementation.Strings.Variable_Text_Handler_Access :=
          VSS.Implementation.Strings.Variable_Handler (Target_Data);

   begin
      if Target_Handler.all in UTF8_In_Place_String_Handler then
         declare
            Target : UTF8_In_Place_String_Handler
              renames UTF8_In_Place_String_Handler (Target_Handler.all);

         begin
            if Target.Size + Size <= In_Place_Storage_Capacity then
               Target.Storage (Target.Size .. Target.Size + Size - 1) :=
                 Storage (From .. From + Size - 1);
               Target.Size   := Target.Size + Size;
               Target.Length := Target.Length + Length;

               if Terminator then
                  Target.Storage (Target.Size) := 16#00#;
               end if;

               Target_Size   := Target.Size;

               return;

            else
               Convert_To_Dynamic (Target, 0, Target.Size + Size);
            end if;
         end;
      end if;

      declare
         Target : UTF8_String_Handler
           renames UTF8_String_Handler (Target_Handler.all);

      begin
         if Target.Pointer.Size + Size > Target.Pointer.Bulk then
            Reallocate (Target.Pointer, 0, Target.Pointer.Size + Size);
         end if;

         Target.Pointer.Storage
           (Target.Pointer.Size .. Target.Pointer.Size + Size - 1) :=
              Storage (From .. From + Size - 1);
         Target.Pointer.Size   := @ + Size;
         Target.Pointer.Length := @ + Length;

         if Terminator then
            Target.Pointer.Storage (Target.Pointer.Size) := 16#00#;
         end if;

         Target_Size := Target.Pointer.Size;
      end;
   end Unchecked_Append;

   ----------------------
   -- Unchecked_Append --
   ----------------------

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False)
   is
      Target_Size : VSS.Unicode.UTF8_Code_Unit_Count;

   begin
      Unchecked_Append
        (Target_Data, Target_Size, Storage, From, Size, Length, Terminator);
   end Unchecked_Append;

   ------------------------
   -- Unchecked_Backward --
   ------------------------

   procedure Unchecked_Backward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position.Index        := Position.Index - 1;
      Position.UTF8_Offset  := Position.UTF8_Offset - 1;
      Position.UTF16_Offset := Position.UTF16_Offset - 1;

      if Position.Index /= 0 then
         loop
            declare
               Code : constant VSS.Unicode.UTF8_Code_Unit :=
                 Storage (Position.UTF8_Offset);

            begin
               case Code is
                  when 16#80# .. 16#BF# =>
                     Position.UTF8_Offset  := Position.UTF8_Offset - 1;

                  when 16#00# .. 16#7F#
                     | 16#C2# .. 16#DF#
                     | 16#E0# .. 16#EF# =>

                     exit;

                  when 16#F0# .. 16#F4# =>
                     Position.UTF16_Offset := Position.UTF16_Offset - 1;
                     exit;

                  when others =>
                     raise Program_Error with "string data is corrupted";
               end case;
            end;
         end loop;
      end if;
   end Unchecked_Backward;

   -----------------------
   -- Unchecked_Forward --
   -----------------------

   procedure Unchecked_Forward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      pragma Suppress (Overflow_Check);
      pragma Suppress (Range_Check);
      --  These checks slowdown execution, but can happen on large text
      --  data or invalid input only. Would be nice to verify that they
      --  are impossible, or modify something to make them impossible.

   begin
      Position.Index := @ + 1;

      if Position.Index = 1 then
         Position.UTF8_Offset  := 0;
         Position.UTF16_Offset := 0;

         return;
      end if;

      declare
         use type Interfaces.Integer_32;
         use type VSS.Unicode.UTF8_Code_Unit;

         --  This code is based on the fact that starting byte of the
         --  multibyte sequence in UTF-8 has N most significant bits set
         --  to one followed by zero bit. So, first byte of the sequence
         --  is negated and number of leading zero bits is counting.

         Code   : constant VSS.Unicode.UTF8_Code_Unit :=
           Storage (Position.UTF8_Offset);
         Length : constant Interfaces.Integer_32 :=
           VSS.Implementation.GCC.clz (Interfaces.Unsigned_32 (not Code))
             - 24;

      begin
         if Code <= 16#7F# then
            Position.UTF8_Offset  := @ + 1;
            Position.UTF16_Offset := @ + 1;

         else
            Position.UTF8_Offset  :=
              @ + VSS.Unicode.UTF8_Code_Unit_Offset (Length);
            Position.UTF16_Offset :=
              @ + VSS.Unicode.UTF16_Code_Unit_Offset (Length / 4 + 1);
         end if;
      end;
   end Unchecked_Forward;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Data : in out UTF8_String_Data_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation
              (UTF8_String_Data, UTF8_String_Data_Access);

   begin
      if Data /= null then
         if System.Atomic_Counters.Decrement (Data.Counter) then
            Free (Data);

         else
            Data := null;
         end if;
      end if;
   end Unreference;

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference
     (Self : in out UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is
   begin
      Unreference (Self.Pointer);
   end Unreference;

   -----------------------
   -- Unsafe_Initialize --
   -----------------------

   procedure Unsafe_Initialize
     (Text     : in out
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Capacity : VSS.Implementation.Strings.Character_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      if Capacity * 4 <= In_Place_Storage_Capacity
        and Size <= In_Place_Storage_Capacity
      then
         declare
            pragma Warnings (Off, """Overlay"" overlays smaller object");
            Overlay : UTF8_In_Place_String_Handler  := (others => <>)
              with Address => Text'Address;
            pragma Warnings (On, """Overlay"" overlays smaller object");

         begin
            null;
         end;

      else
         declare
            pragma Warnings (Off, """Overlay"" overlays smaller object");
            Overlay : UTF8_String_Handler := (others => <>)
              with Address => Text'Address;
            pragma Warnings (On, """Overlay"" overlays smaller object");

         begin
            Overlay.Pointer :=
              Allocate (VSS.Unicode.UTF8_Code_Unit_Count (Capacity) * 4, Size);
         end;
      end if;
   end Unsafe_Initialize;

   -----------------------
   -- Validate_And_Copy --
   -----------------------

   procedure Validate_And_Copy
     (Source      : Ada.Strings.UTF_Encoding.UTF_8_String;
      Destination : out VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length      : out VSS.Implementation.Strings.Character_Count;
      Success     : out Boolean)
   is
      State : Verification_State := Initial;
      Code  : VSS.Unicode.UTF8_Code_Unit;

   begin
      Length := 0;

      for J in Source'Range loop
         Code := Standard.Character'Pos (Source (J));

         case State is
            when Initial =>
               Length := Length + 1;

               case Code is
                  when 16#00# .. 16#7F# =>
                     null;

                  when 16#C2# .. 16#DF# =>
                     State := UT1;

                  when 16#E0# =>
                     State := U31;

                  when 16#E1# .. 16#EC# =>
                     State := UT2;

                  when 16#ED# =>
                     State := U33;

                  when 16#EE# .. 16#EF# =>
                     State := UT2;

                  when 16#F0# =>
                     State := U41;

                  when 16#F1# .. 16#F3# =>
                     State := UT3;

                  when 16#F4# =>
                     State := U43;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U31 =>
               case Code is
                  when 16#A0# .. 16#BF# =>
                     State := UT1;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U33 =>
               case Code is
                  when 16#80# .. 16#9F# =>
                     State := UT1;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U41 =>
               case Code is
                  when 16#90# .. 16#BF# =>
                     State := UT2;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U43 =>
               case Code is
                  when 16#80# .. 16#8F# =>
                     State := UT2;

                  when others =>
                     State := Ill_Formed;
               end case;

            when UT1 =>
               case Code is
                  when 16#80# .. 16#BF# =>
                     State := Initial;

                  when others =>
                     State := Ill_Formed;
               end case;

            when UT2 =>
               case Code is
                  when 16#80# .. 16#BF# =>
                     State := UT1;

                  when others =>
                     State := Ill_Formed;
               end case;

            when UT3 =>
               case Code is
                  when 16#80# .. 16#BF# =>
                     State := UT2;

                  when others =>
                     State := Ill_Formed;
               end case;

            when Ill_Formed =>
               exit;
         end case;

         Destination
           (VSS.Unicode.UTF8_Code_Unit_Count (J - Source'First)) := Code;
      end loop;

      Success := State = Initial;
   end Validate_And_Copy;

end VSS.Implementation.UTF8_String_Handlers;
