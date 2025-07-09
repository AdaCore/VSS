--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Test_Support;

procedure Test_Regexp is
   pragma Style_Checks ("gnaty-s");

   procedure Test_V406_014 is separate;
   procedure Test_V406_018 is separate;
   procedure Test_V426_005 is separate;
   procedure Test_V615_026 is separate;
   procedure Test_Assertion_Quantifier is separate;

   procedure Simple_Tests is
   begin
      Test_Support.Run_Testcase (Test_V406_014'Access, "V406_014");
      Test_Support.Run_Testcase (Test_V406_018'Access, "V406_018");
      Test_Support.Run_Testcase (Test_V426_005'Access, "V426_005");
      Test_Support.Run_Testcase (Test_V615_026'Access, "V615_026");
      Test_Support.Run_Testcase
        (Test_Assertion_Quantifier'Access, "Assertion_Quantifier");
   end Simple_Tests;

begin
   Test_Support.Run_Testsuite (Simple_Tests'Access, "RegExp");
end Test_Regexp;
