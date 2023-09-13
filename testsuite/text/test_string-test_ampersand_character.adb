--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters;

separate (Test_String)
procedure Test_Ampersand_Character is

   use type VSS.Strings.Virtual_String;

begin
   declare
      L : constant VSS.Strings.Virtual_String       := "AB";
      R : constant VSS.Characters.Virtual_Character := 'C';
      V : constant VSS.Strings.Virtual_String       := L & R;

   begin
      Test_Support.Assert (V = "ABC");
   end;

   declare
      R : constant VSS.Strings.Virtual_String       := "BC";
      L : constant VSS.Characters.Virtual_Character := 'A';
      V : constant VSS.Strings.Virtual_String       := L & R;

   begin
      Test_Support.Assert (V = "ABC");
   end;
end Test_Ampersand_Character;
