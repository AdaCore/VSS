--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Character_Iterators;
with Test_Support;

procedure Test_String_Delete is

   use type VSS.Strings.Character_Count;
   use type VSS.Strings.Virtual_String;

   S     : VSS.Strings.Virtual_String := "Hello, world!";
   J0    : constant VSS.Strings.Character_Iterators.Character_Iterator :=
     S.At_First_Character;
   J1    : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.At_First_Character;
   J2    : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.At_Last_Character;
   J3    : constant VSS.Strings.Character_Iterators.Character_Iterator :=
     S.At_Last_Character;
   Dummy : Boolean;

begin
   --  Test result of the delete operation and position of the iterators at
   --  first and last character of the string after operation.

   Dummy := J1.Forward;
   Dummy := J1.Forward;
   Dummy := J1.Forward;
   Dummy := J1.Forward;
   Dummy := J1.Forward;

   Dummy := J2.Backward;

   S.Delete (J1, J2);

   Test_Support.Assert (S = "Hello!");
   Test_Support.Assert (J0.Character_Index = 1);
   Test_Support.Assert (not J1.Is_Valid);
   Test_Support.Assert (not J2.Is_Valid);
   Test_Support.Assert (J3.Character_Index = 6);
end Test_String_Delete;
