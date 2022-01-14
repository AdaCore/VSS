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

with Ada.Wide_Wide_Text_IO;

with VSS.Standard_Paths;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_Standard_Paths is
   Home : constant VSS.Strings.Virtual_String :=
     VSS.Standard_Paths.Writable_Location
       (VSS.Standard_Paths.Home_Location);
   Temp : constant VSS.Strings.Virtual_String :=
     VSS.Standard_Paths.Writable_Location
       (VSS.Standard_Paths.Temp_Location);

begin
   Ada.Wide_Wide_Text_IO.Put_Line
     (VSS.Strings.Conversions.To_Wide_Wide_String (Home));
   Ada.Wide_Wide_Text_IO.Put_Line
     (VSS.Strings.Conversions.To_Wide_Wide_String (Temp));

   Test_Support.Assert (not Home.Is_Empty);
   Test_Support.Assert (not Temp.Is_Empty);
end Test_Standard_Paths;
