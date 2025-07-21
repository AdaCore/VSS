--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Text_Storages.Heap;

package body VSS.Implementation.UTF8_Strings.Mutable_Operations
  with Preelaborate
is

   use type VSS.Implementation.Strings.Character_Offset;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;

   procedure Mutate
     (Text     : in out UTF8_String_Data;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count);

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

   procedure Validate_And_Copy
     (Source      : Ada.Strings.UTF_Encoding.UTF_8_String;
      Destination : out VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length      : out VSS.Implementation.Strings.Character_Count;
      Success     : out Boolean);
   --  Validate UTF-8 encoding and copy validated part of the data to
   --  Destination. Length is set to the length of the text in characters.
   --  Success is set False when validation is failed and to True otherwise.

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

   ------------
   -- Append --
   ------------

   procedure Append
     (Text   : in out UTF8_String_Data;
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

      Mutate (Text, Text.Size + L);

      declare
         Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Text.Size + L)
           with Import, Address => Text.Storage_Address;
      begin
         VSS.Implementation.UTF8_Encoding.Unchecked_Store
           (Storage, Text.Size, L, U1, U2, U3, U4);

         Text.Size           := @ + L;
         Text.Length         := @ + 1;
         Storage (Text.Size) := 16#00#;
      end;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Text : in out UTF8_String_Data;
      Code : VSS.Unicode.Code_Point)
   is
      L  : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
      U1 : VSS.Unicode.UTF8_Code_Unit;
      U2 : VSS.Unicode.UTF8_Code_Unit;
      U3 : VSS.Unicode.UTF8_Code_Unit;
      U4 : VSS.Unicode.UTF8_Code_Unit;

   begin
      VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

      Mutate (Text, Text.Size + L);

      declare
         Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Text.Size + L)
           with Import, Address => Text.Storage_Address;
      begin
         VSS.Implementation.UTF8_Encoding.Unchecked_Store
           (Storage, Text.Size, L, U1, U2, U3, U4);

         Text.Size           := @ + L;
         Text.Length         := @ + 1;
         Storage (Text.Size) := 16#00#;
      end;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Text   : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Suffix : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        Text.Size + Suffix.Size;

   begin
      if Suffix.Size = 0 then
         return;
      end if;

      Mutate (Text, New_Size);

      declare
         Text_Storage   : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. New_Size)
           with Import, Address => Text.Storage_Address;
         Suffix_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Suffix.Size)
           with Import, Address => Suffix.Storage_Address;

      begin
         Internal_Append
           (Storage        => Text_Storage,
            Length         => Text.Length,
            Size           => Text.Size,
            Suffix_Storage => Suffix_Storage,
            Suffix_Length  => Suffix.Length,
            Suffix_Size    => Suffix.Size,
            Offset         => Offset);
      end;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Text        : in out UTF8_String_Data;
      Item_Data   : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Item_Length : VSS.Implementation.Strings.Character_Count)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        Text.Size + Item_Data'Length;

   begin
      Mutate (Text, New_Size);

      declare
         Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. New_Size)
           with Import, Address => Text.Storage_Address;

      begin
         Storage (Text.Size .. New_Size - 1) := Item_Data;

         Text.Size           := New_Size;
         Text.Length         := @ + Item_Length;
         Storage (Text.Size) := 16#00#;
      end;
   end Append;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Text : in out UTF8_String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Offset :=
        Text.Size - Size.UTF8_Offset;

   begin
      if Size.Index_Offset = 0 then
         return;
      end if;

      declare
         Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Text.Size)
           with Import, Address => Text.Storage_Address;

      begin
         Storage (From.UTF8_Offset .. New_Size) :=
           Storage (From.UTF8_Offset + Size.UTF8_Offset .. Text.Size);

         Text.Length := @ - Size.Index_Offset;
         Text.Size   := New_Size;
      end;
   end Delete;

   -----------------------
   -- From_UTF_8_String --
   -----------------------

   procedure From_UTF_8_String
     (Self    : in out UTF8_String_Data;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean) is
   begin
      Initialize (Self, Item'Length);

      declare
         Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Item'Length)
           with Import, Address => Self.Storage_Address;
         Length  : VSS.Implementation.Strings.Character_Count := 0;

      begin
         Validate_And_Copy (Item, Storage, Length, Success);

         if Success then
            Storage
              (VSS.Unicode.UTF8_Code_Unit_Count (Item'Length)) := 16#00#;
            --  GNAT 20240327: compiler crash without type conversion.
            Self.Length := Length;
            Self.Size   := Item'Length;
         end if;
      end;
   end From_UTF_8_String;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   procedure From_Wide_Wide_String
     (Self    : in out UTF8_String_Data;
      Item    : Wide_Wide_String;
      Success : out Boolean)
   is
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Index    : Positive := Item'First;

   begin
      Success := True;

      Initialize (Self, Item'Length);

      if Item'Length <= SSO_Max_Size then
         Capacity := SSO_Max_Size;

      else
         declare
            Manager :
              VSS.Implementation.Text_Storages.Heap.Heap_Storage
              with Import, Address => Self.Manager'Address;

         begin
            Capacity := Manager.Capacity;
         end;
      end if;

      loop
         declare
            Code    : VSS.Unicode.Code_Point;
            L       : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
            U1      : VSS.Unicode.UTF8_Code_Unit;
            U2      : VSS.Unicode.UTF8_Code_Unit;
            U3      : VSS.Unicode.UTF8_Code_Unit;
            U4      : VSS.Unicode.UTF8_Code_Unit;

         begin
            if Index > Item'Last then
               declare
                  Storage : VSS.Implementation.UTF8_Encoding
                    .UTF8_Code_Unit_Array (0 .. Self.Size)
                    with Import, Address => Self.Storage_Address;

               begin
                  Storage (Self.Size) := 16#00#;
                  Self.Length         := Item'Length;

                  exit;
               end;
            end if;

            if Wide_Wide_Character'Pos (Item (Index))
                 not in VSS.Unicode.Code_Point
              or else Wide_Wide_Character'Pos (Item (Index))
                        in 16#D800# .. 16#DFFF#
            then
               Success := False;

               exit;

            else
               Code := Wide_Wide_Character'Pos (Item (Index));
            end if;

            VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

            if Capacity < Self.Size + L then
               Mutate (Self, Self.Size + L);
            end if;

            declare
               Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                 (0 .. Capacity)
                 with Import, Address => Self.Storage_Address;

            begin
               VSS.Implementation.UTF8_Encoding.Unchecked_Store
                 (Storage, Self.Size, L, U1, U2, U3, U4);

               Self.Size   := @ + L;
               Self.Length := @ + 1;

               Index := Index + 1;
            end;
         end;
      end loop;

      if not Success then
         raise Program_Error;
      end if;
   end From_Wide_Wide_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Text     : in out UTF8_String_Data;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      if Capacity <= SSO_Max_Size then
         Text.Manager         := [others => 0];
         Text.Storage_Address := Text.Manager'Address;
         Text.Size            := 0;
         Text.Length          := 0;
         Text.Flags           := 0;

      else
         declare
            Manager :
              VSS.Implementation.Text_Storages.Heap.Heap_Storage :=
                (others => <>)
              with Address => Text.Manager'Address;

         begin
            Manager.Initialize (Text.Storage_Address, Capacity);
            Text.Size   := 0;
            Text.Length := 0;
            Text.Flags  := 1;
         end;
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Text   : in out UTF8_String_Data;
      Data   : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length : VSS.Implementation.Strings.Character_Count) is
   begin
      Initialize (Text, Data'Length - 1);

      declare
         Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Data'Length)
           with Import, Address => Text.Storage_Address;
      begin
         Storage (0 .. Data'Length - 1) := Data;
         Text.Size   := Data'Length;
         Text.Length := Length;
         Storage (Text.Size) := 16#00#;
      end;
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Text   : in out UTF8_String_Data;
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

      Mutate (Text, Text.Size + L);

      declare
         Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Text.Size + L)
           with Import, Address => Text.Storage_Address;
      begin
         Storage
           (From.UTF8_Offset + L .. Text.Size + L) :=
           Storage (From.UTF8_Offset .. Text.Size);

         VSS.Implementation.UTF8_Encoding.Unchecked_Store
           (Storage, From.UTF8_Offset, L, U1, U2, U3, U4);

         Text.Size   := @ + L;
         Text.Length := @ + 1;
         Storage (Text.Size) := 16#00#;
      end;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Text   : in out UTF8_String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      function UTF16_Size
        (Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array)
         return VSS.Unicode.UTF16_Code_Unit_Count;

      ----------------
      -- UTF16_Size --
      ----------------

      function UTF16_Size
        (Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array)
         return VSS.Unicode.UTF16_Code_Unit_Count
      is
         Position : VSS.Unicode.UTF8_Code_Unit_Count  := 0;
         Result   : VSS.Unicode.UTF16_Code_Unit_Count := 0;

      begin
         while Position <= Storage'Last loop
            case Storage (Position) is
               when 16#00# .. 16#7F# =>
                  --  1x UTF8 code units sequence
                  --  => 1x UTF16 code units sequence

                  Position := @ + 1;
                  Result   := @ + 1;

               when 16#C2# .. 16#DF# =>
                  --  2x UTF8 code units sequence
                  --  => 1x UTF16 code units sequence

                  Position := @ + 2;
                  Result   := @ + 1;

               when 16#E0# .. 16#EF# =>
                  --  3x UTF8 code units sequence
                  --  => 1x UTF16 code units sequence

                  Position := @ + 3;
                  Result   := @ + 1;

               when 16#F0# .. 16#F4# =>
                  --  4x code units sequence
                  --  => 2x UTF16 code units sequence

                  Position := @ + 3;
                  Result   := @ + 2;
               when others =>
                  raise Program_Error;
            end case;
         end loop;

         return Result;
      end UTF16_Size;

      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Offset :=
        Text.Size + Item.Size;

   begin
      if Item.Size = 0 then
         return;
      end if;

      if Text.Size = 0 then
         Text := Item;
         VSS.Implementation.UTF8_Strings.Reference (Text);

         return;
      end if;

      Mutate (Text, New_Size);

      declare
         Text_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. New_Size)
           with Import, Address => Text.Storage_Address;
         Item_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Item.Size)
           with Import, Address => Item.Storage_Address;

      begin
         Text_Storage
           (From.UTF8_Offset + Item.Size .. Text.Size + Item.Size) :=
           Text_Storage (From.UTF8_Offset .. Text.Size);
         --  Move NUL terminator too.
         Text_Storage (From.UTF8_Offset .. From.UTF8_Offset + Item.Size - 1) :=
           Item_Storage (0 .. Item.Size - 1);

         Text.Size   := @ + Item.Size;
         Text.Length := @ + Item.Length;

         Offset.Index_Offset := @ + Item.Length;
         Offset.UTF8_Offset  := @ + Item.Size;
         Offset.UTF16_Offset := @ + UTF16_Size (Item_Storage);
      end;
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

   ------------
   -- Mutate --
   ------------

   procedure Mutate
     (Text     : in out UTF8_String_Data;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      if Is_SSO (Text) then
         if Text.Storage_Address = System.Null_Address then
            Text.Storage_Address := Text.Manager'Address;
         end if;

         if Capacity > SSO_Max_Size then
            --  There is no enough space to store data using SSO, convert
            --  data to be managed by heap storage manager.

            declare
               Storage     :
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. SSO_Max_Size)
                 with Import, Address => Text.Storage_Address;
               Aux_Storage : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. SSO_Max_Size) := Storage;
               Manager     :
                 VSS.Implementation.Text_Storages.Heap.Heap_Storage :=
                   (others => <>)
                 with Address => Text.Manager'Address;

            begin
               Manager.Initialize
                 (Text.Storage_Address, Aux_Storage, Capacity);
               Text.Flags := 1;
            end;
         end if;

      else
         declare
            Manager :
              VSS.Implementation.Text_Storages.Abstract_Text_Storage
                with Import, Address => Text.Manager'Address;

         begin
            VSS.Implementation.Text_Storages.Abstract_Text_Storage'Class
              (Manager).Mutate (Text.Storage_Address, Capacity);
         end;
      end if;
   end Mutate;

   ----------------------
   -- Unchecked_Append --
   ----------------------

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
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

   ----------------------
   -- Unchecked_Append --
   ----------------------

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False) is
   begin
      Mutate (Target_Data, Target_Data.Size + Size);

      declare
         Target_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Target_Data.Size + Size)
           with Import, Address => Target_Data.Storage_Address;

      begin
         Target_Storage
           (Target_Data.Size .. Target_Data.Size + Size - 1) :=
              Storage (From .. From + Size - 1);
         Target_Data.Size   := @ + Size;
         Target_Data.Length := @ + Length;

         if Terminator then
            Target_Storage (Target_Data.Size) := 16#00#;
         end if;

         Target_Size := Target_Data.Size;
      end;
   end Unchecked_Append;

   ----------------------
   -- Unchecked_Delete --
   ----------------------

   procedure Unchecked_Delete
     (Target_Data   : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size   : out VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Delete_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_Length : VSS.Implementation.Strings.Character_Count) is
   begin
      declare
         Target_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Target_Data.Size)
           with Import, Address => Target_Data.Storage_Address;

      begin
         if Delete_Size = 0 then
            Target_Size := Target_Data.Size;

         elsif Delete_From + Delete_Size = Target_Data.Size then
            --  End of string is deleted, no data moved, update size and
            --  length only.

            Target_Data.Size   := @ - Delete_Size;
            Target_Data.Length := @ - Delete_Length;

            Target_Size        := Target_Data.Size;

         else
            Target_Storage
              (Delete_From .. Target_Data.Size - Delete_Size - 1) :=
              Target_Storage
                (Delete_From + Delete_Size .. Target_Data.Size - 1);

            Target_Data.Size   := @ - Delete_Size;
            Target_Data.Length := @ - Delete_Length;

            Target_Size        := Target_Data.Size;
         end if;
      end;
   end Unchecked_Delete;

   ----------------------
   -- Unchecked_Insert --
   ----------------------

   procedure Unchecked_Insert
     (Target_Text    : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
      Into           : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Insert_From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Insert_Length  : VSS.Implementation.Strings.Character_Count)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        Target_Text.Size + Insert_Size;

   begin
      Mutate (Target_Text, New_Size);

      declare
         Target_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. Target_Text.Size + New_Size)
           with Import, Address => Target_Text.Storage_Address;

      begin
         Target_Storage
           (Into + Insert_Size .. Target_Text.Size + Insert_Size) :=
           Target_Storage (Into .. Target_Text.Size);
         --  Move NUL terminator too.
         Target_Storage (Into .. Into + Insert_Size - 1) :=
           Insert_Storage (Insert_From .. Insert_From + Insert_Size - 1);

         Target_Text.Size   := @ + Insert_Size;
         Target_Text.Length := @ + Insert_Length;
      end;

      Target_Size := Target_Text.Size;
   end Unchecked_Insert;

   --------------------------
   -- Unchecked_Move_Slice --
   --------------------------

   procedure Unchecked_Move_Slice
     (Text : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index) is
   begin
      if From < Into then
         raise Program_Error;

      elsif Into < From then
         Mutate (Text, Text.Size);

         declare
            Storage   : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
              (0 .. Text.Size)
              with Import, Address => Text.Storage_Address;
            Buffer    : constant
              VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                (0 .. Size - 1) := Storage (From .. From + Size - 1);
            Move_Size : constant VSS.Unicode.UTF8_Code_Unit_Offset :=
              From - Into;

         begin
            Storage (Into + Size .. Into + Size + Move_Size - 1) :=
              Storage (Into .. Into + Move_Size - 1);
            Storage (Into .. Into + Size - 1) := Buffer;
         end;

      else
         null;
      end if;
   end Unchecked_Move_Slice;

   -----------------------
   -- Unchecked_Replace --
   -----------------------

   procedure Unchecked_Replace
     (Target_Data    : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      Insert_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Insert_From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Insert_Length  : VSS.Implementation.Strings.Character_Count)
   is
      New_Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        Target_Data.Size + Insert_Size - Replace_Size;

   begin
      Mutate (Target_Data, New_Size);

      declare
         Target_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
           (0 .. VSS.Unicode.UTF8_Code_Unit_Count'Max
                   (Target_Data.Size, New_Size))
           with Import, Address => Target_Data.Storage_Address;

      begin
         Target_Storage (Replace_From + Insert_Size .. New_Size) :=
           Target_Storage (Replace_From + Replace_Size .. Target_Data.Size);
         --  Move NUL terminator too.
         Target_Storage (Replace_From .. Replace_From + Insert_Size - 1) :=
           Insert_Storage (Insert_From .. Insert_From + Insert_Size - 1);

         Target_Data.Size   := New_Size;
         Target_Data.Length := @ + Insert_Length - Replace_Length;
      end;

      Target_Size := Target_Data.Size;
   end Unchecked_Replace;

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

end VSS.Implementation.UTF8_Strings.Mutable_Operations;
