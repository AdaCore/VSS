--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Strings.Character_Iterators;
with Test_Support;

separate (Test_String)
procedure Test_Delete is
begin
   declare
      use type VSS.Strings.Character_Count;

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
   end;

   --  VSS#278
   --
   --  Check that `Delete` mutate shared text data, thus other objects are not
   --  modified.

   declare
      S          : VSS.Strings.Virtual_String :=
        "      Ada_With_Private_Absent,";
      SB         : constant VSS.Strings.Virtual_String := S;
      S_Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_First_Character;
      E_Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_Last_Character;

   begin
      Test_Support.Assert (S_Iterator.Forward);
      Test_Support.Assert (S_Iterator.Forward);
      Test_Support.Assert (S_Iterator.Forward);
      Test_Support.Assert (S_Iterator.Forward);
      Test_Support.Assert (S_Iterator.Forward);
      Test_Support.Assert (S_Iterator.Forward);

      Test_Support.Assert (E_Iterator.Backward);

      S.Delete (S_Iterator, E_Iterator);

      Test_Support.Assert (SB = "      Ada_With_Private_Absent,");
      Test_Support.Assert (S = "      ,");
   end;
end Test_Delete;
