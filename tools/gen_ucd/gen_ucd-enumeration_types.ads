--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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

   procedure Initialize
     (Self     : in out Enumeration_Type'Class;
      Property : not null UCD.Properties.Property_Access;
      Zero     : Wide_Wide_String);
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

   function Hash (Item : Natural) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Item));

   package Integer_Property_Value_Maps is
     new Ada.Containers.Hashed_Maps
       (Natural,
        UCD.Properties.Property_Value_Access,
        Hash,
        "=",
        UCD.Properties."=");

   type Enumeration_Type is tagged limited record
      Property          : UCD.Properties.Property_Access;
      To_Representation : Property_Value_Integer_Maps.Map;
      To_Value          : Integer_Property_Value_Maps.Map;
   end record;

end Gen_UCD.Enumeration_Types;
