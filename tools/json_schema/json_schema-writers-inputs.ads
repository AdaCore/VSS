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

with JSON_Schema.Readers;

package JSON_Schema.Writers.Inputs is

   procedure Generate_Readers
     (Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Header         : VSS.String_Vectors.Virtual_String_Vector;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set);
   --  Generate Write routines types for schemas in given Map

end JSON_Schema.Writers.Inputs;
