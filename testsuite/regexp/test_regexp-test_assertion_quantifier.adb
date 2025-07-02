--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Regular_Expressions;
pragma Warnings (Off, "is not referenced");
with VSS.Strings.Character_Iterators;

separate (Test_Regexp)
procedure Test_Assertion_Quantifier is

   R : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("\b+");

begin
   Test_Support.Assert (not R.Is_Valid);
end Test_Assertion_Quantifier;