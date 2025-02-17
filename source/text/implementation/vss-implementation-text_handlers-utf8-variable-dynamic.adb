--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Unchecked_Deallocation;

with VSS.Implementation.Text_Handlers.UTF8.Variable.Static;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic is

   use type VSS.Unicode.UTF16_Code_Unit_Offset;

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

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : Dynamic_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position :=
        (Index        => Self.Pointer.Length + 1,
         UTF8_Offset  => Self.Pointer.Size,
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
     (Self   : in out Dynamic_UTF8_Handler;
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
     (Self   : in out Dynamic_UTF8_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Parent : VSS.Implementation.Text_Handlers.Abstract_Text_Handler
        renames VSS.Implementation.Text_Handlers.Abstract_Text_Handler (Self);

      Suffix_Handler : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Suffix);

   begin
      if Suffix_Handler.all in Static.Static_UTF8_Handler then
         --  Suffix is static storage.

         declare
            Suffix_Static : Static.Static_UTF8_Handler
              renames Static.Static_UTF8_Handler (Suffix_Handler.all);

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

      elsif Suffix_Handler.all in Dynamic_UTF8_Handler then
         --  Suffix is dynamic storage.

         declare
            Suffix_Dynamic : Dynamic_UTF8_Handler
              renames Dynamic_UTF8_Handler (Suffix_Handler.all);

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

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self     : Dynamic_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      if Position.Index = 0 then
         return False;

      else
         Unchecked_Backward (Self.Pointer.Storage, Position);
      end if;

      return Position.Index > 0;
   end Backward;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   overriding procedure Before_First_Character
     (Self     : Dynamic_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position := (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   end Before_First_Character;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out Dynamic_UTF8_Handler;
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

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : Dynamic_UTF8_Handler;
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
   -- Forward --
   -------------

   overriding function Forward
     (Self     : Dynamic_UTF8_Handler;
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

   ---------------------
   -- Forward_Element --
   ---------------------

   overriding function Forward_Element
     (Self     : Dynamic_UTF8_Handler;
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

   -----------------------
   -- From_UTF_8_String --
   -----------------------

   overriding procedure From_UTF_8_String
     (Self    : in out Dynamic_UTF8_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean)
   is
      Pointer : UTF8_String_Data_Access renames Self.Pointer;
      Length  : VSS.Implementation.Strings.Character_Count := 0;

   begin
      Validate_And_Copy (Item, Pointer.Storage, Length, Success);

      if Success then
         Pointer.Storage
           (VSS.Unicode.UTF8_Code_Unit_Count (Item'Length)) := 16#00#;
         --  GNAT 20240327: compiler crash without type conversion.
         Pointer.Length := Length;
         Pointer.Size   := Item'Length;

      else
         Unreference (Pointer);
      end if;
   end From_UTF_8_String;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   overriding procedure From_Wide_Wide_String
     (Self    : in out Dynamic_UTF8_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean) is
   begin
      Success := True;

      declare
         Pointer : UTF8_String_Data_Access renames Self.Pointer;
         Code    : VSS.Unicode.Code_Point;
         L       : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
         U1      : VSS.Unicode.UTF8_Code_Unit;
         U2      : VSS.Unicode.UTF8_Code_Unit;
         U3      : VSS.Unicode.UTF8_Code_Unit;
         U4      : VSS.Unicode.UTF8_Code_Unit;

      begin
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

         else
            Unreference (Pointer);
         end if;
      end;
   end From_Wide_Wide_String;

   -------------------
   -- Has_Character --
   -------------------

   overriding function Has_Character
     (Self     : Dynamic_UTF8_Handler;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      return Position.Index in 1 .. Self.Pointer.Length;
   end Has_Character;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out Dynamic_UTF8_Handler;
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
         VSS.Unicode.UTF8_Code_Unit_Count (Self.Unsafe_Capacity) * 4,
         Self.Pointer.Size + L);

      Self.Pointer.Storage
        (From.UTF8_Offset + L .. Self.Pointer.Size + L) :=
           Self.Pointer.Storage (From.UTF8_Offset .. Self.Pointer.Size);

      VSS.Implementation.UTF8_Encoding.Unchecked_Store
        (Self.Pointer.Storage, From.UTF8_Offset, L, U1, U2, U3, U4);

      Self.Pointer.Size   := @ + L;
      Self.Pointer.Length := @ + 1;
      Self.Pointer.Storage (Self.Pointer.Size) := 16#00#;
      --  XXX Is it necessary? NUL is copied by move of storage data
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Self : Dynamic_UTF8_Handler) return Boolean is
   begin
      return Self.Pointer.Length = 0;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : Dynamic_UTF8_Handler)
      return VSS.Implementation.Strings.Character_Count is
   begin
      return Self.Pointer.Length;
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

   overriding procedure Reference (Self : in out Dynamic_UTF8_Handler) is
   begin
      System.Atomic_Counters.Increment (Self.Pointer.Counter);
   end Reference;

   -----------
   -- Slice --
   -----------

   overriding procedure Slice
     (Self        : Dynamic_UTF8_Handler;
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

      if To.Index <= Self.Length then
         Unchecked_Forward (Self.Pointer.Storage, After);
      end if;

      Size   := After.UTF8_Offset - From.UTF8_Offset;
      Length := After.Index - From.Index;

      if Size <= Static.In_Place_Storage_Capacity then
         declare
            Static : Variable.Static.Static_UTF8_Handler := (others => <>)
              with Address => Target_Data.Storage'Address;

         begin
            Static.Storage (0 .. Size - 1) :=
              Self.Pointer.Storage (From.UTF8_Offset .. After.UTF8_Offset - 1);
            Static.Size   := Size;
            Static.Length := Length;
            Static.Storage (Static.Size) := 16#00#;
         end;

      else
         declare
            Pointer : UTF8_String_Data_Access;
            Dynamic : Dynamic_UTF8_Handler := (others => <>)
              with Address => Target_Data.Storage'Address;

         begin
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

   -----------------
   -- Split_Lines --
   -----------------

   overriding procedure Split_Lines
     (Self            : Dynamic_UTF8_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access) is
   begin
      Split_Lines_Common
        (Text            => Self,
         Data            => Data,
         Storage         => Self.Pointer.Storage,
         Terminators     => Terminators,
         Keep_Terminator => Keep_Terminator,
         Lines           => Lines);
   end Split_Lines;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : Dynamic_UTF8_Handler)
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

   overriding procedure Unreference (Self : in out Dynamic_UTF8_Handler) is
   begin
      Unreference (Self.Pointer);
   end Unreference;

   ------------------------------------
   -- UTF8_Constant_Storage_And_Size --
   ------------------------------------

   overriding procedure UTF8_Constant_Storage_And_Size
     (Self    : Dynamic_UTF8_Handler;
      Pointer : out
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : out VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      Pointer := Self.Pointer.Storage (Self.Pointer.Storage'First)'Access;
      Size    := Self.Pointer.Size;
   end UTF8_Constant_Storage_And_Size;

   ----------------------------------
   -- UTF8_Constant_Storage_Poiner --
   ----------------------------------

   overriding function UTF8_Constant_Storage_Poiner
     (Self : Dynamic_UTF8_Handler)
      return not null
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access is
   begin
      return Self.Pointer.Storage (Self.Pointer.Storage'First)'Access;
   end UTF8_Constant_Storage_Poiner;

   -----------------------
   -- UTF8_Insert_Slice --
   -----------------------

   overriding procedure UTF8_Insert_Slice
     (Self    : in out Dynamic_UTF8_Handler;
      Into    : VSS.Unicode.UTF8_Code_Unit_Index;
      Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Length  : VSS.Implementation.Strings.Character_Count) is
   begin
      Mutate
        (Self.Pointer,
         VSS.Unicode.UTF8_Code_Unit_Count (Self.Unsafe_Capacity) * 4,
         Self.Pointer.Size + Size);

      Self.Pointer.Storage (Into + Size .. Self.Pointer.Size + Size) :=
        Self.Pointer.Storage (Into .. Self.Pointer.Size);
      --  Move NUL terminator too.
      Self.Pointer.Storage (Into .. Into + Size - 1) :=
        Storage (From .. From + Size - 1);

      Self.Pointer.Size   := @ + Size;
      Self.Pointer.Length := @ + Length;
   end UTF8_Insert_Slice;

   ---------------
   -- UTF8_Move --
   ---------------

   overriding procedure UTF8_Move
     (Self : in out Dynamic_UTF8_Handler;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index) is
   begin
      raise Program_Error;
   end UTF8_Move;

   ------------------------
   -- UTF8_Replace_Slice --
   ------------------------

   overriding procedure UTF8_Replace_Slice
     (Self           : in out Dynamic_UTF8_Handler;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      By_Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      By_From        : VSS.Unicode.UTF8_Code_Unit_Index;
      By_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      By_Length      : VSS.Implementation.Strings.Character_Count)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        Self.Pointer.Size + By_Size - Replace_Size;

   begin
      Mutate
        (Self.Pointer,
         VSS.Unicode.UTF8_Code_Unit_Count (Self.Unsafe_Capacity) * 4,
         New_Size);

      Self.Pointer.Storage (Replace_From + By_Size .. New_Size) :=
        Self.Pointer.Storage
          (Replace_From + Replace_Size .. Self.Pointer.Size);
      --  Move NUL terminator too.
      Self.Pointer.Storage (Replace_From .. Replace_From + By_Size - 1) :=
        By_Storage (By_From .. By_From + By_Size - 1);

      Self.Pointer.Size   := New_Size;
      Self.Pointer.Length := @ + By_Length - Replace_Length;
   end UTF8_Replace_Slice;

   ---------------
   -- UTF8_Size --
   ---------------

   overriding function UTF8_Size
     (Self : Dynamic_UTF8_Handler) return VSS.Unicode.UTF8_Code_Unit_Count is
   begin
      return Self.Pointer.Size;
   end UTF8_Size;

end VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic;
