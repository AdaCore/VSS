------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

--  with VSS.Characters;
with VSS.Strings.Character_Iterators;

with Test_Support;

procedure Test_String_Replace is

   use type VSS.Strings.Virtual_String;

   S  : VSS.Strings.Virtual_String := "Hello, bad world!";
   J1 : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.At_First_Character;
   J2 : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.At_Last_Character;

begin
   Test_Support.Assert (J1.Forward);
   Test_Support.Assert (J1.Forward);
   Test_Support.Assert (J1.Forward);
   Test_Support.Assert (J1.Forward);
   Test_Support.Assert (J1.Forward);
   Test_Support.Assert (J1.Forward);
   Test_Support.Assert (J1.Forward);

   Test_Support.Assert (J2.Backward);
   Test_Support.Assert (J2.Backward);
   Test_Support.Assert (J2.Backward);
   Test_Support.Assert (J2.Backward);
   Test_Support.Assert (J2.Backward);
   Test_Support.Assert (J2.Backward);
   Test_Support.Assert (J2.Backward);

   S.Replace (J1, J2, "good");

   Test_Support.Assert (S = "Hello, good world!");
end Test_String_Replace;
