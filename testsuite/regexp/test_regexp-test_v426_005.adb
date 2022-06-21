--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Regular_Expressions;
with VSS.Strings;

separate (Test_Regexp)
procedure Test_V426_005 is

   use type VSS.Strings.Virtual_String;

   --  Check a negated character classes regexp.

   R1 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("a[^\p{digit}a-fx]b");
   R2 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression
       ("a[^\P{Lowercase_Letter}x]b");
   M : VSS.Regular_Expressions.Regular_Expression_Match;

   A1B : constant VSS.Strings.Virtual_String := "a1b";
   ACB : constant VSS.Strings.Virtual_String := "acb";
   AXB : constant VSS.Strings.Virtual_String := "axb";
   AGB : constant VSS.Strings.Virtual_String := "aGb";
begin
   M := R1.Match (A1B);
   Test_Support.Assert (not M.Has_Match);
   M := R1.Match (ACB);
   Test_Support.Assert (not M.Has_Match);
   M := R1.Match (AXB);
   Test_Support.Assert (not M.Has_Match);
   M := R1.Match (AGB);
   Test_Support.Assert (M.Has_Match and then M.Captured = "aGb");

   M := R2.Match (A1B);
   Test_Support.Assert (not M.Has_Match);
   M := R2.Match (ACB);
   Test_Support.Assert (M.Has_Match and then M.Captured = "acb");
   M := R2.Match (AXB);
   Test_Support.Assert (not M.Has_Match);
   M := R2.Match (AGB);
   Test_Support.Assert (not M.Has_Match);

end Test_V426_005;
