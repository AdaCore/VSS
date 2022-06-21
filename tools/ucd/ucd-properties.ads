--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

package UCD.Properties is

   type Canonical_Combinig_Class is range 0 .. 255;

   package String_Vectors is
     new Ada.Containers.Vectors (Positive, Unbounded_Wide_Wide_String);

   type Property_Value is record
      Names                           : String_Vectors.Vector;
      Is_Used                         : Boolean := False;
      Canonical_Combining_Class_Value : Canonical_Combinig_Class;
      String                          : Code_Point_Vectors.Vector;
   end record;

   type Property_Value_Access is access all Property_Value;

   package Property_Value_Vectors is
      new Ada.Containers.Vectors (Positive, Property_Value_Access);

   package String_Property_Value_Maps is
     new Ada.Containers.Hashed_Maps
       (Unbounded_Wide_Wide_String,
        Property_Value_Access,
        Wide_Wide_Hash,
        "=");

   type Property is record
      Names         : String_Vectors.Vector;
      All_Values    : Property_Value_Vectors.Vector;
      Name_To_Value : String_Property_Value_Maps.Map;

      Is_Canonical_Combining_Class : Boolean := False;
      Is_Binary                    : Boolean := False;
      Is_Enumeration               : Boolean := False;
      Is_String                    : Boolean := False;
   end record;

   type Property_Access is access all Property;

   package Property_Vectors is
     new Ada.Containers.Vectors (Positive, Property_Access);

   package Name_Property_Maps is
     new Ada.Containers.Hashed_Maps
       (Unbounded_Wide_Wide_String, Property_Access, Wide_Wide_Hash, "=");

   All_Properties   : Property_Vectors.Vector;
   Name_To_Property : Name_Property_Maps.Map;

   function Resolve
     (Property_Name : Wide_Wide_String) return not null Property_Access;

   function Resolve
     (Property   : not null Property_Access;
      Value_Name : Wide_Wide_String) return Property_Value_Access;

   function Hash
     (Item : Property_Value_Access) return Ada.Containers.Hash_Type;

end UCD.Properties;
