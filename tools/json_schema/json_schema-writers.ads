--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Hashed_Sets;
with VSS.Strings.Hash;
with JSON_Schema.Readers;

package JSON_Schema.Writers is

   subtype Schema_Name is VSS.Strings.Virtual_String;
   --  Name of the schema. It looks like "#/definitions/Response"

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
        (Name     : Schema_Name;
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
      Name   : Schema_Name;
      Schema : Schema_Access;
      Action : access procedure
        (Enclosing : Schema_Name;
         Property  : JSON_Schema.Property;
         Required  : Boolean));
   --  For given Schema traverse allOf items, collect all properties and call
   --  the Action procedure for each such property. Name is the Schema name.
   --  Enclosing is the name of schema for the Property.

   procedure Each_Anonymous_Schema
     (Map    : JSON_Schema.Readers.Schema_Map;
      Schema : Schema_Access;
      Action : access procedure (Property : JSON_Schema.Property));
   --  Call Action for each schema nested in given Schema

   procedure Each_Union_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Optional : String_Sets.Set;
      Action   : access procedure
        (Name     : Schema_Name;
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
      Action   : access procedure (Name : Schema_Name));
   --  Execute Action on each schema used in Holders

   function Ref_To_Type_Name (Subschema : Schema_Name)
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
     (Name      : Schema_Name;
      Map       : JSON_Schema.Readers.Schema_Map;
      Prop      : Property;
      Type_Name : out VSS.Strings.Virtual_String;
      Prefix    : out VSS.Strings.Virtual_String);

   procedure Print_Vector (Header : VSS.String_Vectors.Virtual_String_Vector);

   function Variant_Name
     (Map       : JSON_Schema.Readers.Schema_Map;
      Schema    : Schema_Access) return VSS.Strings.Virtual_String;
   --  Return a variant name for given Schema when schema is an element of
   --  anyOf schema

   function Is_Holder_Field
     (Name     : Schema_Name;
      Property : VSS.Strings.Virtual_String;
      Holders  : VSS.String_Vectors.Virtual_String_Vector) return Boolean;
   --  Check if given Property in the schema with Name should be presented as a
   --  holder type

   function Is_Union_Type (Schema : Schema_Access) return Boolean;
   --  Some of `anyOf` schema is not {} or has $ref

   function Is_Enum (Schema : Schema_Access) return Boolean is
     (not Schema.Enum.Is_Empty or not Schema.X_Enum.Is_Empty);
   --  Check for enumeration schema

end JSON_Schema.Writers;
