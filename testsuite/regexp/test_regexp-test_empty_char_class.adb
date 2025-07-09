--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Regular_Expressions;
with VSS.Strings;

separate (Test_Regexp)
procedure Test_Empty_Char_Class is
   R1 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("a[]b");

   Text : constant VSS.Strings.Virtual_String := "ab";

   M  : constant VSS.Regular_Expressions.Regular_Expression_Match :=
     R1.Match (Text);
begin
   Test_Support.Assert (R1.Is_Valid);
   Test_Support.Assert (not M.Has_Match);
end Test_Empty_Char_Class;
