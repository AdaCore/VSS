--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Application;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_Transformer is

   UCD_Root : constant Wide_Wide_String :=
     VSS.Strings.Conversions.To_Wide_Wide_String
       (VSS.Application.Arguments.Element (1));

   --  Testsuites

   procedure Test_Normalization;

   --  Testcases

   procedure Test_UCD_NormalizationTest;

   ------------------------
   -- Test_Normalization --
   ------------------------

   procedure Test_Normalization is
   begin
      Test_Support.Run_Testcase
        (Test_UCD_NormalizationTest'Access,
        "UCD NormalizationTest.txt");
   end Test_Normalization;

   --------------------------------
   -- Test_UCD_NormalizationTest --
   --------------------------------

   procedure Test_UCD_NormalizationTest is separate;

begin
   Test_Support.Run_Testsuite
     (Test_Normalization'Access, "Normalization Transformation");
end Test_Transformer;
