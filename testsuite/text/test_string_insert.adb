--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_String_Insert is

   procedure Test_Insert_Character;
   procedure Test_Insert_String;

   ---------------------------
   -- Test_Insert_Character --
   ---------------------------

   procedure Test_Insert_Character is
      E : constant Wide_Wide_String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

      S : VSS.Strings.Virtual_String := "Z";
      J : constant VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_First_Character;

   begin
      --  Test result insert of the character operation, as well as correct
      --  tracking of the iterator location on insert operation.

      for K in VSS.Characters.Virtual_Character'('A') .. 'Y' loop
         S.Insert (J, K);

         Test_Support.Assert
           (VSS.Strings.Conversions.To_Wide_Wide_String (S)
            = E (E'First .. E'First
              + (VSS.Characters.Virtual_Character'Pos (K)
                - VSS.Characters.Virtual_Character'Pos ('A'))) & 'Z');
      end loop;
   end Test_Insert_Character;

   ------------------------
   -- Test_Insert_String --
   ------------------------

   procedure Test_Insert_String is
      use type VSS.Strings.Character_Index;
      use type VSS.Strings.Virtual_String;

      S : VSS.Strings.Virtual_String := "Hello!";
      J : constant VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_Last_Character;

   begin
      S.Insert (J, ", world");

      Test_Support.Assert (S = "Hello, world!");
      Test_Support.Assert (J.Character_Index = 13);
   end Test_Insert_String;

begin
   Test_Insert_Character;
   Test_Insert_String;
end Test_String_Insert;
