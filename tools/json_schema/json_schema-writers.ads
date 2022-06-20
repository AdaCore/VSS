--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Sets;
with VSS.Strings.Hash;
with JSON_Schema.Readers;

package JSON_Schema.Writers is

   package String_Sets is new Ada.Containers.Hashed_Sets
     (VSS.Strings.Virtual_String,
      VSS.Strings.Hash,
      VSS.Strings."=",
      VSS.Strings."=");

   type Declaration_Kind is (Specification, Implemenetation);

   procedure Put (Text : VSS.Strings.Virtual_String);
   --  Write Text to stdout

   procedure New_Line;
   --  Write a line terminator to stdout

   function Escape_Keywords (Name : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String;
   --  Add a prefix when Name is in the Reserved_Words list

   procedure Each_Enumeration_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Optional : String_Sets.Set;
      Action   : access procedure
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean));
   --  Find enumeration schemas and call Action for them. Optional is set of
   --  schema name those are used in not-required properties. The Action
   --  procedure takes next arguments:
   --  * Name - name of toppest named schema containing enumeration Schema
   --  * Property - name of property if enumeration schema is nameless (when
   --    schema declaration is embedded into a property declaration).
   --  * Schema - corresponding enumeration schema
   --  * Optional - True if enumeration schema is used in not-required property

   procedure Each_Property
     (Map    : JSON_Schema.Readers.Schema_Map;
      Schema : Schema_Access;
      Action : access procedure
        (Property : JSON_Schema.Property;
         Required : Boolean));
   --  For given Schema traverse allOf items, collect all properties and call
   --  the Action procedure for each such property.

   procedure Each_Anonymous_Schema
     (Map    : JSON_Schema.Readers.Schema_Map;
      Schema : Schema_Access;
      Action : access procedure (Property : JSON_Schema.Property));
   --  Call Action for each schema nested in given Schema

   procedure Each_Union_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Optional : String_Sets.Set;
      Action   : access procedure
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean));
   --  Find `anyOf` schemas and call Action for them. Optional is set of
   --  schema name those are used in not-required properties. The Action
   --  procedure takes next arguments:
   --  * Name - name of toppest named schema containing anyOf Schema
   --  * Property - name of property if anyOf schema is nameless (when
   --    schema declaration is embedded into a property declaration).
   --  * Schema - corresponding anyOf schema
   --  * Optional - True if anyOf schema is used in not-required property

   procedure Each_Holder_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Holders  : VSS.String_Vectors.Virtual_String_Vector;
      Action   : access procedure
        (Name : VSS.Strings.Virtual_String));
   --  Execute Action on each schema used in Holders

   function Ref_To_Type_Name (Subschema : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String;
   --  Convert $ref to a type name

   procedure Get_Field_Type
     (Map       : JSON_Schema.Readers.Schema_Map;
      Schema    : Schema_Access;
      Required  : Boolean;
      Fallback  : VSS.Strings.Virtual_String;
      Type_Name : out VSS.Strings.Virtual_String;
      Prefix    : out VSS.Strings.Virtual_String);
   --  Return an Ada type name for given Schema. Fallback if a type name for
   --  properties with nested schema declaration.
   --  Return an empty string for string properties with just one enumeration
   --  literal defined, so we can skip such properties in the type declaration.
   --  If type is defined in a package, then return package name in Prefix.

   procedure Get_Element_Type
     (Map       : JSON_Schema.Readers.Schema_Map;
      Schema    : Schema_Access;
      Type_Name : out VSS.Strings.Virtual_String;
      Prefix    : out VSS.Strings.Virtual_String);

   procedure Print_Vector (Header : VSS.String_Vectors.Virtual_String_Vector);

   function Variant_Name
     (Map       : JSON_Schema.Readers.Schema_Map;
      Schema    : Schema_Access) return VSS.Strings.Virtual_String;
   --  Return a variant name for given Schema when schema is an element of
   --  anyOf schema

   function Is_Holder_Field
     (Name     : VSS.Strings.Virtual_String;
      Property : VSS.Strings.Virtual_String;
      Holders  : VSS.String_Vectors.Virtual_String_Vector) return Boolean;
   --  Check if given Property in Schema should be presented as a holder type

end JSON_Schema.Writers;
