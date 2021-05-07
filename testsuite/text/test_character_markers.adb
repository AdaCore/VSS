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

with Ada.Containers.Vectors;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Markers;

procedure Test_Character_Markers is
   use type VSS.Strings.Character_Count;

   procedure Test_Append;
   procedure Test_Insert;
   procedure Test_Vectors_Finalization;

   -----------------
   -- Test_Append --
   -----------------

   procedure Test_Append is
      S1  : VSS.Strings.Virtual_String := "Hello, ";
      M11 : constant VSS.Strings.Markers.Character_Marker :=
        S1.First_Character.Marker;
      M12 : constant VSS.Strings.Markers.Character_Marker :=
        S1.Last_Character.Marker;

   begin
      S1.Append ("world");

      if M11.Character_Index /= 1 then
         raise Program_Error;
      end if;

      if M12.Character_Index /= 7 then
         raise Program_Error;
      end if;

      S1.Append ('!');

      if M11.Character_Index /= 1 then
         raise Program_Error;
      end if;

      if M12.Character_Index /= 7 then
         raise Program_Error;
      end if;
   end Test_Append;

   -----------------
   -- Test_Insert --
   -----------------

   procedure Test_Insert is
      S1  : VSS.Strings.Virtual_String := "AC";
      J1  : VSS.Strings.Character_Iterators.Character_Iterator :=
        S1.First_Character;
      M11 : constant VSS.Strings.Markers.Character_Marker :=
        S1.First_Character.Marker;
      M12 : constant VSS.Strings.Markers.Character_Marker :=
        S1.Last_Character.Marker;

      S2  : VSS.Strings.Virtual_String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      J2  : VSS.Strings.Character_Iterators.Character_Iterator :=
        S2.First_Character;
      M21 : constant VSS.Strings.Markers.Character_Marker :=
        S2.First_Character.Marker;
      M22 : constant VSS.Strings.Markers.Character_Marker :=
        S2.Last_Character.Marker;

   begin
      if not J1.Forward then
         raise Program_Error;
      end if;

      if M11.Character_Index /= 1 then
         raise Program_Error;
      end if;

      if M12.Character_Index /= 2 then
         raise Program_Error;
      end if;

      S1.Insert (J1, 'B');

      if M11.Character_Index /= 1 then
         raise Program_Error;
      end if;

      if M12.Character_Index /= 3 then
         raise Program_Error;
      end if;

      if not J2.Forward then
         raise Program_Error;
      end if;

      if not J2.Forward then
         raise Program_Error;
      end if;

      if M21.Character_Index /= 1 then
         raise Program_Error;
      end if;

      if M22.Character_Index /= 26 then
         raise Program_Error;
      end if;

      S2.Insert (J2, '1');

      if M21.Character_Index /= 1 then
         raise Program_Error;
      end if;

      if M22.Character_Index /= 27 then
         raise Program_Error;
      end if;

   end Test_Insert;

   -------------------------------
   -- Test_Vectors_Finalization --
   -------------------------------

   procedure Test_Vectors_Finalization is

      package Marker_Vectors is new Ada.Containers.Vectors
        (Natural,
         VSS.Strings.Markers.Character_Marker,
         VSS.Strings.Markers."=");

      S  : constant VSS.Strings.Virtual_String := "Hello, world!";
      J1 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
        S.First_Character with Unreferenced;
      J2 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
        S.Last_Character with Unreferenced;

   begin
      --  Test finalization of the markers inside the vector container.

      declare
         V : Marker_Vectors.Vector;

      begin
         V.Append (S.First_Character.Marker);
         V.Append (S.Last_Character.Marker);
      end;
   end Test_Vectors_Finalization;

begin
   Test_Append;
   Test_Insert;
   Test_Vectors_Finalization;
end Test_Character_Markers;
