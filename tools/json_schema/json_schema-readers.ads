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

with Ada.Containers.Hashed_Maps;
with VSS.JSON.Pull_Readers;
with VSS.Strings.Hash;

package JSON_Schema.Readers is

   type Schema_Version is (Draft_4, Draft_6, Draft_7);

   package Schema_Maps is new Ada.Containers.Hashed_Maps
     (VSS.Strings.Virtual_String,
      Schema_Access,
      VSS.Strings.Hash,
      VSS.Strings."=");

   type Schema_Map is new Schema_Maps.Map with null record;

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Schema  : out Schema_Access;
      Other   : out Schema_Map;
      Version : Schema_Version := Schema_Version'Last);
   --  Parse JSON using given Reader and fill a Schema object. Put sub-schemas
   --  into the Other name-to-schema map.

end JSON_Schema.Readers;
