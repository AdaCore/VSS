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
with VSS.Strings;
with VSS.Strings.Cursors.Iterators.Characters;

separate (Test_Regexp)
procedure Test_V615_026 is
   --  Check Anchored_Match option.

   R1 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("bc");
   R2 : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression ("^bc");
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

   if Pos.Forward then  --  Skip `a`
      M := R2.Match (ABC, Pos);
      Test_Support.Assert (M.Has_Match);
   else
      raise Program_Error;
   end if;
end Test_V615_026;
