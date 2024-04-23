--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Interfaces;

with VSS.Implementation.GCC;
with VSS.Implementation.Line_Iterators;
with VSS.Implementation.Text_Handlers.UTF8.Dynamic;
with VSS.Implementation.Text_Handlers.UTF8.Static;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.UTF8 is

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

         elsif Size <= Static.In_Place_Storage_Capacity then
            --  Static storage can be used.

            declare
               Static : UTF8.Static.Static_UTF8_Handler := (others => <>)
                 with Address => Data.Storage'Address;

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
               Pointer : UTF8.Dynamic.UTF8_String_Data_Access;
               Dynamic : UTF8.Dynamic.Dynamic_UTF8_Handler := (others => <>)
                 with Address => Data.Storage'Address;

            begin
               Pointer := UTF8.Dynamic.Allocate (0, Size);

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
      if Target_Handler.all in Static.Static_UTF8_Handler then
         declare
            Target : Static.Static_UTF8_Handler
              renames Static.Static_UTF8_Handler (Target_Handler.all);

         begin
            if Target.Size + Size <= Static.In_Place_Storage_Capacity then
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
               Unsafe_Convert_To_Dynamic (Target, 0, Target.Size + Size);
            end if;
         end;
      end if;

      declare
         Target : Dynamic.Dynamic_UTF8_Handler
           renames Dynamic.Dynamic_UTF8_Handler (Target_Handler.all);

      begin
         if Target.Pointer.Size + Size > Target.Pointer.Bulk then
            Dynamic.Reallocate (Target.Pointer, 0, Target.Pointer.Size + Size);
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

   -------------------------------
   -- Unsafe_Convert_To_Dynamic --
   -------------------------------

   procedure Unsafe_Convert_To_Dynamic
     (Text     : in out
        VSS.Implementation.Text_Handlers.UTF8.Static.Static_UTF8_Handler;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      Pointer : constant Dynamic.UTF8_String_Data_Access :=
        Dynamic.Allocate (Capacity, Size);

   begin
      Pointer.Storage (0 .. Text.Size) := Text.Storage (0 .. Text.Size);
      Pointer.Length                   := Text.Length;
      Pointer.Size                     := Text.Size;

      declare
         Overlay : Dynamic.Dynamic_UTF8_Handler := (others => <>)
           with Address => Text'Address;

      begin
         Overlay := (Pointer => Pointer);
      end;
   end Unsafe_Convert_To_Dynamic;

   -----------------------
   -- Unsafe_Initialize --
   -----------------------

   procedure Unsafe_Initialize
     (Text     : in out
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Capacity : VSS.Implementation.Strings.Character_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      if Capacity * 4 <= Static.In_Place_Storage_Capacity
        and Size <= Static.In_Place_Storage_Capacity
      then
         declare
            pragma Warnings (Off, """Overlay"" overlays smaller object");
            Overlay : Static.Static_UTF8_Handler  := (others => <>)
              with Address => Text'Address;
            pragma Warnings (On, """Overlay"" overlays smaller object");

         begin
            null;
         end;

      else
         declare
            pragma Warnings (Off, """Overlay"" overlays smaller object");
            Overlay : Dynamic.Dynamic_UTF8_Handler := (others => <>)
              with Address => Text'Address;
            pragma Warnings (On, """Overlay"" overlays smaller object");

         begin
            Overlay.Pointer :=
              Dynamic.Allocate
                (VSS.Unicode.UTF8_Code_Unit_Count (Capacity) * 4, Size);
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

end VSS.Implementation.Text_Handlers.UTF8;
