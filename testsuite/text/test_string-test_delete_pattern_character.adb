--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

separate (Test_String)
procedure Test_Delete_Pattern_Character is
   use type VSS.Strings.Virtual_String;

begin
   --  Null and empty strings.

   declare
      SN : VSS.Strings.Virtual_String;
      SE : VSS.Strings.Virtual_String := "";

   begin
      Test_Support.Assert (SN.Delete (' ').Is_Empty);
      Test_Support.Assert (SN.Delete (' ').Is_Null);

      Test_Support.Assert (SE.Delete (' ').Is_Empty);
      Test_Support.Assert (SE.Delete (' ').Is_Null);
   end;

   --  Usual cases

   declare
      S1 : constant VSS.Strings.Virtual_String := "A,B,C";
      S2 : constant VSS.Strings.Virtual_String := ",,,";
      S3 : constant VSS.Strings.Virtual_String := ",A,B,C";
      S4 : constant VSS.Strings.Virtual_String := "A,B,C,";
      S5 : constant VSS.Strings.Virtual_String := "A,B,,C";
      S6 : constant VSS.Strings.Virtual_String := ",,A,B,C";
      S7 : constant VSS.Strings.Virtual_String := "A,B,C,,";

   begin
      Test_Support.Assert (S1.Delete (' ') = S1);
      Test_Support.Assert (S1.Delete (',') = "ABC");

      Test_Support.Assert (S2.Delete (' ') = S2);
      Test_Support.Assert (S2.Delete (',').Is_Empty);
      Test_Support.Assert (S2.Delete (',').Is_Null);

      Test_Support.Assert (S3.Delete (',') = "ABC");
      Test_Support.Assert (S4.Delete (',') = "ABC");
      Test_Support.Assert (S5.Delete (',') = "ABC");
      Test_Support.Assert (S6.Delete (',') = "ABC");
      Test_Support.Assert (S7.Delete (',') = "ABC");
   end;
end Test_Delete_Pattern_Character;
