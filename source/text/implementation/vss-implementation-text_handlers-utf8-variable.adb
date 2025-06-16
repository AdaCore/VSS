--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic;
with VSS.Implementation.Text_Handlers.UTF8.Variable.Static;

package body VSS.Implementation.Text_Handlers.UTF8.Variable is

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
      if Target_Handler.all in Variable.Static.Static_UTF8_Handler then
         declare
            Target : Variable.Static.Static_UTF8_Handler
              renames Variable.Static.Static_UTF8_Handler (Target_Handler.all);

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
               Unsafe_Convert_To_Dynamic (Target, Target.Size + Size);
            end if;
         end;
      end if;

      declare
         Target : Variable.Dynamic.Dynamic_UTF8_Handler
           renames Variable.Dynamic.Dynamic_UTF8_Handler (Target_Handler.all);

      begin
         if Target.Size + Size > Target.Pointer.Bulk then
            Dynamic.Reallocate
              (Target.Storage, Target.Pointer, Target.Size + Size);
         end if;

         Target.Pointer.Storage
           (Target.Size .. Target.Size + Size - 1) :=
              Storage (From .. From + Size - 1);
         Target.Size   := @ + Size;
         Target.Length := @ + Length;

         if Terminator then
            Target.Pointer.Storage (Target.Size) := 16#00#;
         end if;

         Target_Size := Target.Size;
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

   -------------------------------
   -- Unsafe_Convert_To_Dynamic --
   -------------------------------

   procedure Unsafe_Convert_To_Dynamic
     (Text : in out
        VSS.Implementation.Text_Handlers.UTF8.Variable.Static
          .Static_UTF8_Handler;
      Size : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      Text_Size   : constant VSS.Unicode.UTF8_Code_Unit_Count           :=
        Text.Size;
      Text_Length : constant VSS.Implementation.Strings.Character_Count :=
        Text.Length;
      Pointer     : constant Variable.Dynamic.UTF8_String_Data_Access :=
        Variable.Dynamic.Allocate (Size);

   begin
      Pointer.Storage (0 .. Text.Size) := Text.Storage (0 .. Text.Size);

      declare
         Overlay : Variable.Dynamic.Dynamic_UTF8_Handler := (others => <>)
           with Address => Text'Address;

      begin
         Overlay :=
           (Length  => Text_Length,
            Size    => Text_Size,
            Storage =>
              Pointer.Storage (Pointer.Storage'First)'Unchecked_Access,
            Pointer => Pointer);
      end;
   end Unsafe_Convert_To_Dynamic;

   -----------------------
   -- Unsafe_Initialize --
   -----------------------

   procedure Unsafe_Initialize
     (Text : in out
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class;
      Size : VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      if Size <= Variable.Static.In_Place_Storage_Capacity then
         declare
            pragma Warnings (Off, """Overlay"" overlays smaller object");
            Overlay : Variable.Static.Static_UTF8_Handler  := (others => <>)
              with Address => Text'Address;
            pragma Warnings (On, """Overlay"" overlays smaller object");

         begin
            null;
         end;

      else
         declare
            pragma Warnings (Off, """Overlay"" overlays smaller object");
            Overlay : Variable.Dynamic.Dynamic_UTF8_Handler := (others => <>)
              with Address => Text'Address;
            pragma Warnings (On, """Overlay"" overlays smaller object");

         begin
            Overlay.Pointer := Variable.Dynamic.Allocate (Size);
            Overlay.Storage :=
              Overlay.Pointer.Storage
                (Overlay.Pointer.Storage'First)'Unchecked_Access;
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

end VSS.Implementation.Text_Handlers.UTF8.Variable;
