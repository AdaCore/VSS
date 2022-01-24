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
with VSS.Environments;

with VSS.String_Vectors;
with VSS.Strings;

with Test_Support;

procedure Test_Environment is
   use type VSS.Strings.Virtual_String;

begin
   --  Undefined environment variable

   declare
      V0 : constant VSS.Strings.Virtual_String :=
        VSS.Application.System_Environment.Value ("VSS_ENV0");

   begin
      Test_Support.Assert (V0.Is_Null);
   end;

   --  Defined environment variable, value contains platform specific
   --  path separator thus can't be checked.

   declare
      V1 : constant VSS.Strings.Virtual_String :=
        VSS.Application.System_Environment.Value ("VSS_ENV1");

   begin
      Test_Support.Assert (not V1.Is_Null);
      Test_Support.Assert (not V1.Is_Empty);
   end;

   --  Defined environment variable as list of paths.

   declare
      VP1 : constant VSS.String_Vectors.Virtual_String_Vector :=
        VSS.Application.System_Environment.Value_Paths ("VSS_ENV1");

   begin
      Test_Support.Assert (VP1.Length = 3);
      Test_Support.Assert (VP1 (1) = "A");
      Test_Support.Assert (VP1 (2) = "B");
      Test_Support.Assert (VP1 (3) = "C");
   end;
end Test_Environment;
