--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Regular_Expressions;
with VSS.Strings.Character_Iterators;

separate (Test_Regexp)
procedure Test_V406_014 is

   use type VSS.Strings.Virtual_String;

   --  Check what Last_Marker can be used to obtain slice from the subject
   --  string object.

   S : constant VSS.Strings.Virtual_String := "@param A";
   R : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("@(param|return)");
   M : VSS.Regular_Expressions.Regular_Expression_Match;

begin
   M := R.Match (S);

   Test_Support.Assert
     (S.Slice (M.Last_Marker, S.At_Last_Character) = "m A");
end Test_V406_014;
