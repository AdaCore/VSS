--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Hashed_Maps;
with Ada.Wide_Wide_Text_IO;

with UCD.Properties;

package Gen_UCD.Enumeration_Types is

   type Enumeration_Type is tagged limited private;

   procedure Initialize
     (Self     : in out Enumeration_Type'Class;
      Property : not null UCD.Properties.Property_Access);
   --  Initialize mapping of used enumeration property values to continuous
   --  range of natural numbers used for internal representation.

   function Representation
     (Self : Enumeration_Type'Class;
      Code : UCD.Code_Point) return Natural;
   --  Return internal representation of the given value of the property.

   procedure Generate_Type_Declaration
     (Self : Enumeration_Type'Class;
      File : Ada.Wide_Wide_Text_IO.File_Type);
   --  Generate enumeration type declaration and necessary representation
   --  clauses.

private

   package Property_Value_Integer_Maps is
     new Ada.Containers.Hashed_Maps
       (UCD.Properties.Property_Value_Access,
        Natural,
        UCD.Properties.Hash,
        UCD.Properties."=");

   type Enumeration_Type is tagged limited record
      Property : UCD.Properties.Property_Access;
      Map      : Property_Value_Integer_Maps.Map;
   end record;

end Gen_UCD.Enumeration_Types;
