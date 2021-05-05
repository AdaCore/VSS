------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_String_Insert is

   E : constant Wide_Wide_String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   --  S1 : VSS.Strings.Virtual_String := "C";

   S : VSS.Strings.Virtual_String := "Z";
   J : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.First_Character;

begin
   --  Test result insert of the character operation, as well as correct
   --  tracking of the iterator location on insert operation.

   for K in VSS.Characters.Virtual_Character'('A') .. 'Y' loop
      S.Insert (J, K);

      Test_Support.Assert
        (VSS.Strings.Conversions.To_Wide_Wide_String (S)
         = E (E'First .. E'First
                + (VSS.Characters.Virtual_Character'Pos (K)
                     - VSS.Characters.Virtual_Character'Pos ('A'))) & 'Z');
   end loop;

end Test_String_Insert;
