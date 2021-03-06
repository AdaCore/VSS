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
--  Simple test for Virtual_String.Slice subprograms.
--
--  This test need to be enhanced to check corner cases and special conditions,
--  at least:
--   - invalid cursors
--   - misuse of cursor for another string

with VSS.Strings.Character_Iterators;

with Test_Support;

procedure Test_String_Slice is
   use type VSS.Strings.Virtual_String;

   S  : VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("ASCII Кириллица ⊗∬ 𝛻𝜕 ");
   S1 : VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("A");
   S2 : VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("ASCII");
   S3 : VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("Кириллица");
   S4 : VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("⊗∬ 𝛻𝜕 ");

   J1 : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.First_Character;
   J2 : VSS.Strings.Character_Iterators.Character_Iterator :=
     S.First_Character;
   D  : Boolean;

begin
   Test_Support.Assert (S.Slice (J1, J2) = S1);
   Test_Support.Assert (S.Slice (J1) = S1);

   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;

   Test_Support.Assert (S.Slice (J2, J1).Is_Empty);
   Test_Support.Assert (S.Slice (J1, J2) = S2);

   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;

   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;

   Test_Support.Assert (S.Slice (J1, J2) = S3);

   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;
   D := J1.Forward;

   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;
   D := J2.Forward;

   Test_Support.Assert (S.Slice (J1, J2) = S4);
end Test_String_Slice;
