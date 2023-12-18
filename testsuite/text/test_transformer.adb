--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Test driver requires command line parameters:
--    1) Path to UCD database
--    2..*) Paths to W3C I18N test files

with VSS.Application;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_Transformer is

   UCD_Root      : constant Wide_Wide_String :=
     VSS.Strings.Conversions.To_Wide_Wide_String
       (VSS.Application.Arguments.Element (1));

   W3C_I18N_File : VSS.Strings.Virtual_String;
   --  Path to test data file. Used by Test_Casing_W3C_I18N subprogram.

   --  Testsuites

   procedure Test_Normalization;
   procedure Test_Casing;

   --  Testcases

   procedure Test_UCD_NormalizationTest;
   procedure Test_Casing_Minimal;
   procedure Test_Casing_W3C_I18N;

   -----------------
   -- Test_Casing --
   -----------------

   procedure Test_Casing is
   begin
      Test_Support.Run_Testcase
        (Test_Casing_Minimal'Access,
         "Minimal case conversions");

      for J in 2 .. VSS.Application.Arguments.Length loop
         W3C_I18N_File := VSS.Application.Arguments.Element (J);

         Test_Support.Run_Testcase
           (Test_Casing_Minimal'Access,
            "W3C I18N case conversions ("
            & VSS.Strings.Conversions.To_UTF_8_String (W3C_I18N_File)
            & ")");
      end loop;
   end Test_Casing;

   -------------------------
   -- Test_Casing_Minimal --
   -------------------------

   procedure Test_Casing_Minimal is separate;

   --------------------------
   -- Test_Casing_W3C_I18N --
   --------------------------

   procedure Test_Casing_W3C_I18N is separate;

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
   Test_Support.Run_Testsuite
     (Test_Casing'Access, "Case Conversion Transformation");
end Test_Transformer;
