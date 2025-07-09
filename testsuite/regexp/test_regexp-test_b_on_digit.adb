--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Regular_Expressions;
with VSS.Strings;
with VSS.Strings.Cursors;

separate (Test_Regexp)
procedure Test_B_On_Digit is
   use type VSS.Strings.Character_Count;
   use type VSS.Strings.Virtual_String;

   R1 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("\b5\b");

   Text : constant VSS.Strings.Virtual_String := "15 5 25";

   M  : constant VSS.Regular_Expressions.Regular_Expression_Match :=
     R1.Match (Text);
begin
   Test_Support.Assert (M.Has_Match);

   Test_Support.Assert
     (VSS.Strings.Cursors.Abstract_Cursor'Class
        (M.Marker).First_Character_Index = 4);

   Test_Support.Assert (M.Captured = "5");
end Test_B_On_Digit;
