--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with UCD.Properties;

package UCD.Characters is

   type Character_Information is private;

   procedure Initialize_Character_Database;

   procedure Set
     (Character : Code_Point;
      Property  : not null UCD.Properties.Property_Access;
      Value     : not null UCD.Properties.Property_Value_Access);

   function Get
     (Character : Code_Point;
      Property  : not null UCD.Properties.Property_Access)
      return UCD.Properties.Property_Value_Access;

private

   type Character_Record;

   type Character_Information is access all Character_Record;

end UCD.Characters;
