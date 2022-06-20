--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Test_Support;

procedure Test_Regexp is
   pragma Style_Checks ("gnaty-s");

   procedure Test_V406_014 is separate;
   procedure Test_V406_018 is separate;
   procedure Test_V426_005 is separate;
   procedure Test_V615_026 is separate;

begin
   Test_V406_014;
   Test_V406_018;
   Test_V426_005;
   Test_V615_026;
end Test_Regexp;
