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

with VSS.Strings.Character_Iterators;
with Test_Support;

procedure Test_String_Delete is

   use type VSS.Strings.Character_Count;
   use type VSS.Strings.Virtual_String;

   S     : VSS.Strings.Virtual_String := "Hello, world!";
   J0    : constant VSS.Strings.Character_Iterators.Character_Iterator :=
     S.First_Character;
   J1    : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.First_Character;
   J2    : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.Last_Character;
   J3    : constant VSS.Strings.Character_Iterators.Character_Iterator :=
     S.Last_Character;
   Dummy : Boolean;

begin
   --  Test result of the delete operation and position of the iterators at
   --  first and last character of the string after operation.

   Dummy := J1.Forward;
   Dummy := J1.Forward;
   Dummy := J1.Forward;
   Dummy := J1.Forward;
   Dummy := J1.Forward;

   Dummy := J2.Backward;

   S.Delete (J1, J2);

   Test_Support.Assert (S = "Hello!");
   Test_Support.Assert (J0.Character_Index = 1);
   Test_Support.Assert (not J1.Is_Valid);
   Test_Support.Assert (not J2.Is_Valid);
   Test_Support.Assert (J3.Character_Index = 6);
end Test_String_Delete;
