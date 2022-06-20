--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
