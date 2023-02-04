--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Regular_Expressions;
with VSS.Strings;
with VSS.Strings.Cursors.Iterators.Characters;

separate (Test_Regexp)
procedure Test_V615_026 is
   --  Check Anchored_Match option.

   R1 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("bc");
   R2 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("^bc");
   R3 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("(b)c");
   M : VSS.Regular_Expressions.Regular_Expression_Match;

   X : constant VSS.Regular_Expressions.Match_Options :=
     (VSS.Regular_Expressions.Anchored_Match => True);

   ABC : constant VSS.Strings.Virtual_String := "abc";
   BCD : constant VSS.Strings.Virtual_String := "bcd";
   BC  : constant VSS.Strings.Virtual_String := "bc";
   Pos : VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
     ABC.At_First_Character;
begin
   M := R1.Match (ABC, X);
   Test_Support.Assert (not M.Has_Match);
   M := R1.Match (BCD, X);
   Test_Support.Assert (not M.Has_Match);
   M := R1.Match (BC, X);
   Test_Support.Assert (M.Has_Match);
   M := R2.Match (BC);
   Test_Support.Assert (M.Has_Match);
   M := R3.Match (BC, X);
   Test_Support.Assert (M.Has_Match);
   M := R3.Match (ABC, X);
   Test_Support.Assert (not M.Has_Match);

   if Pos.Forward then  --  Skip `a`
      M := R2.Match (ABC, Pos);
      Test_Support.Assert (M.Has_Match);
   else
      raise Program_Error;
   end if;
end Test_V615_026;
