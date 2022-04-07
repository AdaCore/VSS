------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

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
