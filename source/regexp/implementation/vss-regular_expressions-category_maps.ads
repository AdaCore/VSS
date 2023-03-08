--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with VSS.Strings.Hash;
with VSS.Regular_Expressions.Name_Sets;

private
package VSS.Regular_Expressions.Category_Maps is
   pragma Preelaborate;

   package Maps is new Ada.Containers.Hashed_Maps
     (VSS.Strings.Virtual_String,
      VSS.Regular_Expressions.Name_Sets.General_Category_Set,
      VSS.Strings.Hash,
      VSS.Strings."=",
      VSS.Regular_Expressions.Name_Sets."=");

   procedure Init (Map : out Maps.Map);
end VSS.Regular_Expressions.Category_Maps;
