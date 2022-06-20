--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Regular_Expressions;
with VSS.Strings.Character_Iterators;

separate (Test_Regexp)
procedure Test_V406_018 is

   use type VSS.Strings.Virtual_String;

   --  Check what Last_Marker can be used to obtain slice from the subject
   --  string object.

   S : constant VSS.Strings.Virtual_String := "6πR²";
   R : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression
       ("[\p{L}\p{Nl}][\p{L}\p{Nl}\p{Mn}\p{Mc}\p{Nd}\p{Pc}]*");
   M : VSS.Regular_Expressions.Regular_Expression_Match;

begin
   Test_Support.Assert (R.Is_Valid);

   M := R.Match (S);

   Test_Support.Assert (M.Has_Match);
   Test_Support.Assert (S.Slice (M.First_Marker, M.Last_Marker) = "πR");
end Test_V406_018;
