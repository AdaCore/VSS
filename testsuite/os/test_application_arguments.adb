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

with VSS.Application;
with VSS.String_Vectors;
with VSS.Strings;

with Test_Support;

procedure Test_Application_Arguments is

   use type VSS.Strings.Virtual_String;

   Args : constant VSS.String_Vectors.Virtual_String_Vector :=
     VSS.Application.Arguments;

begin
   Test_Support.Assert (Args.Length = 4);
   Test_Support.Assert (Args (1) = "hello");
   Test_Support.Assert (Args (2) = "привет");
   Test_Support.Assert (Args (3) = "გამარჯობა");
   Test_Support.Assert (Args (4) = "👋");
end Test_Application_Arguments;
