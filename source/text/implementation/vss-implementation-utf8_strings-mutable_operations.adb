--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Storage_Managers.Heap;

package body VSS.Implementation.UTF8_Strings.Mutable_Operations
  with Preelaborate
is

   use type VSS.Implementation.Strings.Character_Offset;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;

   procedure Mutate
     (Text     : in out UTF8_String_Data;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count);

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
           (0 .. Text.Size + New_Size)
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
              VSS.Implementation.Storage_Managers.Heap.Heap_Storage_Manager
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
              VSS.Implementation.Storage_Managers.Heap.Heap_Storage_Manager :=
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
                 VSS.Implementation.Storage_Managers.Heap
                   .Heap_Storage_Manager := (others => <>)
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
              VSS.Implementation.Storage_Managers.Abstract_Storage_Manager
                with Import, Address => Text.Manager'Address;

         begin
            VSS.Implementation.Storage_Managers.Abstract_Storage_Manager'Class
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

end VSS.Implementation.UTF8_Strings.Mutable_Operations;
