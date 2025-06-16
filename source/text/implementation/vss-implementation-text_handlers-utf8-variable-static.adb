--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.UTF8.Variable.Static is

   use type VSS.Unicode.UTF16_Code_Unit_Offset;

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : Static_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position :=
        (Index        => Self.Length + 1,
         UTF8_Offset  => Self.Size,
         UTF16_Offset => 0);
   end After_Last_Character;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Static_UTF8_Handler;
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
         Unsafe_Convert_To_Dynamic (Self, Self.Size + L);

         declare
            Overlay : Variable.Dynamic.Dynamic_UTF8_Handler
              renames Variable.Dynamic.Dynamic_UTF8_Handler
              (VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
                 (Self));
            Pointer : Variable.Dynamic.UTF8_String_Data_Access
              renames Overlay.Pointer;

         begin
            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Pointer.Storage,
               Pointer.Size, L, U1, U2, U3, U4);

            Pointer.Size   := @ + L;
            Pointer.Length := @ + 1;
            Pointer.Storage (Pointer.Size) := 16#00#;
         end;
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Static_UTF8_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Parent : VSS.Implementation.Text_Handlers.Abstract_Text_Handler
        renames VSS.Implementation.Text_Handlers.Abstract_Text_Handler
                  (Self);

      Suffix_Handler : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Suffix);

   begin
      if Suffix_Handler.all in Variable.Dynamic.Dynamic_UTF8_Handler then
         --  The suffix use dynamic storage. However, in some cases, result
         --  might be stored in the current static storage.

         declare
            Suffix_Dynamic : Variable.Dynamic.Dynamic_UTF8_Handler
              renames Variable.Dynamic.Dynamic_UTF8_Handler
                        (Suffix_Handler.all);
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

               Unsafe_Convert_To_Dynamic
                 (Self, Self.Size + Suffix_Dynamic.Pointer.Size);

               Handler := VSS.Implementation.Strings.Variable_Handler (Data);
               Handler.Append (Data, Suffix, Offset);
            end if;
         end;

      elsif Suffix_Handler.all in Static_UTF8_Handler then
         declare
            Suffix_Static : Static_UTF8_Handler
              renames Static_UTF8_Handler (Suffix_Handler.all);
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

               Unsafe_Convert_To_Dynamic
                 (Self, Self.Size + Suffix_Static.Size);

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
     (Self     : Static_UTF8_Handler;
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
     (Self     : Static_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position := (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   end Before_First_Character;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out Static_UTF8_Handler;
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
     (Self     : Static_UTF8_Handler;
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
     (Self     : Static_UTF8_Handler;
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
     (Self     : Static_UTF8_Handler;
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
     (Self    : in out Static_UTF8_Handler;
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
     (Self    : in out Static_UTF8_Handler;
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
               Unsafe_Initialize (Self, Self.Size + L);

               VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
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
     (Self     : Static_UTF8_Handler;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      return Position.Index in 1 .. Self.Length;
   end Has_Character;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out Static_UTF8_Handler;
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

         Unsafe_Convert_To_Dynamic (Self, Self.Size + L);

         declare
            Text : Variable.Dynamic.Dynamic_UTF8_Handler
              with Import, Convention => Ada, Address => Self'Address;

         begin
            Text.Pointer.Storage
              (From.UTF8_Offset + L .. Text.Pointer.Size + L) :=
                 Text.Pointer.Storage (From.UTF8_Offset .. Text.Pointer.Size);

            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Text.Pointer.Storage, From.UTF8_Offset, L, U1, U2, U3, U4);

            Text.Pointer.Size   := @ + L;
            Text.Pointer.Length := @ + 1;
            Text.Pointer.Storage (Text.Pointer.Size) := 16#00#;
         end;
      end if;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : Static_UTF8_Handler) return Boolean is
   begin
      return Self.Length = 0;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : Static_UTF8_Handler)
      return VSS.Implementation.Strings.Character_Count is
   begin
      return Self.Length;
   end Length;

   -----------
   -- Slice --
   -----------

   overriding procedure Slice
     (Self        : Static_UTF8_Handler;
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
         Unchecked_Forward (Self.Storage, After);
      end if;

      Size   := After.UTF8_Offset - From.UTF8_Offset;
      Length := After.Index - From.Index;

      declare
         Static : Static_UTF8_Handler
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
     (Self            : Static_UTF8_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access) is
   begin
      Split_Lines_Common
        (Text            => Self,
         Data            => Data,
         Storage         => Self.Storage,
         Terminators     => Terminators,
         Keep_Terminator => Keep_Terminator,
         Lines           => Lines);
   end Split_Lines;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : Static_UTF8_Handler)
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

   ------------------------------------
   -- UTF8_Constant_Storage_And_Size --
   ------------------------------------

   overriding procedure UTF8_Constant_Storage_And_Size
     (Self    : Static_UTF8_Handler;
      Pointer : out
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : out VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      Pointer := Self.Storage (Self.Storage'First)'Unchecked_Access;
      Size    := Self.Size;
   end UTF8_Constant_Storage_And_Size;

   ----------------------------------
   -- UTF8_Constant_Storage_Poiner --
   ----------------------------------

   overriding function UTF8_Constant_Storage_Poiner
     (Self : Static_UTF8_Handler)
      return not null
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access is
   begin
      return Self.Storage (Self.Storage'First)'Unchecked_Access;
   end UTF8_Constant_Storage_Poiner;

   -----------------------
   -- UTF8_Insert_Slice --
   -----------------------

   overriding procedure UTF8_Insert_Slice
     (Self    : in out Static_UTF8_Handler;
      Into    : VSS.Unicode.UTF8_Code_Unit_Index;
      Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Length  : VSS.Implementation.Strings.Character_Count) is
   begin
      if Self.Size + Size <= In_Place_Storage_Capacity then
         Self.Storage (Into + Size .. Self.Size + Size) :=
           Self.Storage (Into .. Self.Size);
         --  Move NUL terminator too.
         Self.Storage (Into .. Into + Size - 1) :=
           Storage (From .. From + Size - 1);

         Self.Size   := @ + Size;
         Self.Length := @ + Length;

      else
         --  Size of the current static storge is not enough, move current text
         --  into dynamic storage, and call handler of the dynamic storage to
         --  complete operation.

         Unsafe_Convert_To_Dynamic (Self, Self.Size + Size);

         declare
            Text : Variable.Dynamic.Dynamic_UTF8_Handler
              with Import, Convention => Ada, Address => Self'Address;

         begin
            Text.UTF8_Insert_Slice
              (Into    => Into,
               Storage => Storage,
               From    => From,
               Size    => Size,
               Length  => Length);
         end;
      end if;
   end UTF8_Insert_Slice;

   ---------------
   -- UTF8_Move --
   ---------------

   overriding procedure UTF8_Move
     (Self : in out Static_UTF8_Handler;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index) is
   begin
      if From < Into then
         raise Program_Error;

      elsif Into < From then
         declare
            Buffer    : constant
              VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                (0 .. Size - 1) := Self.Storage (From .. From + Size - 1);
            Move_Size : constant VSS.Unicode.UTF8_Code_Unit_Offset :=
              From - Into;

         begin
            Self.Storage (Into + Size .. Into + Size + Move_Size - 1) :=
              Self.Storage (Into .. Into + Move_Size - 1);
            Self.Storage (Into .. Into + Size - 1) := Buffer;
         end;

      else
         null;
      end if;
   end UTF8_Move;

   ------------------------
   -- UTF8_Replace_Slice --
   ------------------------

   overriding procedure UTF8_Replace_Slice
     (Self           : in out Static_UTF8_Handler;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      By_Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      By_From        : VSS.Unicode.UTF8_Code_Unit_Index;
      By_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      By_Length      : VSS.Implementation.Strings.Character_Count) is
   begin
      if Self.Size - Replace_Size + By_Size <= In_Place_Storage_Capacity then
         Self.Storage
           (Replace_From + By_Size .. Self.Size - Replace_Size + By_Size) :=
              Self.Storage (Replace_From + Replace_Size .. Self.Size);
         --  Move NUL terminator too.
         Self.Storage (Replace_From .. Replace_From + By_Size - 1) :=
           By_Storage (By_From .. By_From + By_Size - 1);

         Self.Size   := @ + By_Size - Replace_Size;
         Self.Length := @ + By_Length - Replace_Length;

      else
         --  Size of the current static storge is not enough, move current text
         --  into dynamic storage, and call handler of the dynamic storage to
         --  complete operation.

         Unsafe_Convert_To_Dynamic
           (Self, Self.Size - Replace_Size + By_Size);

         declare
            Text : Variable.Dynamic.Dynamic_UTF8_Handler
              with Import, Convention => Ada, Address => Self'Address;

         begin
            Text.UTF8_Replace_Slice
              (Replace_From   => Replace_From,
               Replace_Size   => Replace_Size,
               Replace_Length => Replace_Length,
               By_Storage     => By_Storage,
               By_From        => By_From,
               By_Size        => By_Size,
               By_Length      => By_Length);
         end;
      end if;
   end UTF8_Replace_Slice;

   ---------------
   -- UTF8_Size --
   ---------------

   overriding function UTF8_Size
     (Self : Static_UTF8_Handler) return VSS.Unicode.UTF8_Code_Unit_Count is
   begin
      return Self.Size;
   end UTF8_Size;

end VSS.Implementation.Text_Handlers.UTF8.Variable.Static;
