--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

separate (Test_String)
procedure Test_Asterisk_Character is
begin
   declare
      R : constant VSS.Strings.Virtual_String := 0 * 'A';

   begin
      Test_Support.Assert (R.Is_Empty);
      --  Test_Support.Assert (not R.Is_Null);
      --  XXX Should it be non-null string?
   end;

   declare
      R : constant VSS.Strings.Virtual_String := 5 * 'B';

   begin
      Test_Support.Assert (not R.Is_Empty);
      Test_Support.Assert (R = "BBBBB");
   end;
end Test_Asterisk_Character;
