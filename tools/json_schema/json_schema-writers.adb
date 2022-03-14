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

pragma Ada_2022;

with Ada.Containers.Hashed_Sets;
with Ada.Wide_Wide_Text_IO;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Strings.Hash;

package body JSON_Schema.Writers is

   package String_Sets is new Ada.Containers.Hashed_Sets
     (VSS.Strings.Virtual_String,
      VSS.Strings.Hash,
      VSS.Strings."=",
      VSS.Strings."=");

   procedure Write_Named_Types
     (Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  Generate types for all named schemas in Map if they not present in Done
   --  already. Include names of generated type in Done set.

   procedure Write_Named_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  Generate a type for single named Schema. Generate Optional_ type if
   --  corresponding type is included in Optional_Types set. Update Done as
   --  described above.

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property_Name  : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set;
      Required       : Boolean);
   --  Write a dedicated type for a schema property that contains a nested
   --  schema. Currently only single level of nesting is implemented.

   procedure Write_Record_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  The same for "type: object" schema

   procedure Write_Record_Component
     (Name     : VSS.Strings.Virtual_String;
      Map      : JSON_Schema.Readers.Schema_Map;
      Property : JSON_Schema.Property;
      Required : Boolean);
   --  Write record component declaration for given Property. Name is a name of
   --  the schema containing the property.

   procedure Write_Derived_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  The same for "allOf:[]" schema

   procedure Write_Enumeration_Type
     (Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access);
   --  The same for enumeration schema

   procedure Write_Enumeration_Types
     (Name      : VSS.Strings.Virtual_String;
      Property  : VSS.Strings.Virtual_String;
      Schema    : Schema_Access;
      Required  : Boolean);
   --  Traverse all types in Schema recursively, find enumeration schemes
   --  and write corresponding Ada types. Name is top named schema, while
   --  Property (if not empty) is a corresponding property.

   procedure Write_Public_Vectors (Array_Types : Readers.Schema_Map);
   --  Write vector type public declarations for each item of Array_Types

   type Declaration_Kind is (Specification, Implemenetation);

   procedure Write_Vector_Operations
     (Array_Types : Readers.Schema_Map;
      Kind        : Declaration_Kind);
   --  Write vector type operations for each item of Array_Types

   procedure Write_Private_Vectors (Array_Types : Readers.Schema_Map);
   --  Write vector type private declarations for each item of Array_Types

   function Escape_Keywords (Name : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String;
   --  Add a prefix when Name is in the Reserved_Words list

   function Ref_To_Type_Name (Subschema : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String;
   --  Convert $ref to a type name

   procedure Find_Optional_Types
     (Schema : Schema_Access;
      Result : in out String_Sets.Set);
   --  Scan Schema recursively and find all types that are referenced in
   --  optional properties.

   procedure Find_Array_Types
     (Map    : JSON_Schema.Readers.Schema_Map;
      Schema : Schema_Access;
      Result : in out Readers.Schema_Map);
   --  Scan Schema recursively and find all types that are referenced as items
   --  in an array schema.

   procedure Put (Text : VSS.Strings.Virtual_String);
   --  Write Text to stdout
   procedure New_Line;
   --  Write a line terminator to stdout

   procedure Write_Comment
     (Description : VSS.Strings.Virtual_String;
      Indent      : Natural);
   --  Write Description as an Ada comment (handle LF if any).
   --  Use Indent spaces before comment markers.

   procedure Write_Optional_Type (Name : VSS.Strings.Virtual_String);
   --  Write an Optional_<Name> type for given type Name

   procedure Write_Any_Object (Name : VSS.Strings.Virtual_String);
   --  Write an Ada type that represents an unrestricted object

   procedure Write_Type_Package (Map : JSON_Schema.Readers.Schema_Map);
   --  Write package specificatio with type declarations

   function Is_Enum (Schema : Schema_Access) return Boolean is
     (not Schema.Enum.Is_Empty);
   --  Check for enumeration schema

   function Field_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Schema   : Schema_Access;
      Required : Boolean;
      Fallback : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Return an Ada type name for given Schema. Fallback if a type name for
   --  properties with nested schema declaration.
   --  Return an empty string for string properties with just one enumeration
   --  literal defined, so we can skip such properties in the type declaration.

   Reserved_Words : constant VSS.String_Vectors.Virtual_String_Vector :=
   ["function", "interface", "all", "type", "body",
     "private", "protected", "new", "exception", "entry", "goto", "end",
     "boolean"];

   Integer_Or_String : constant JSON_Schema.Simple_Type_Vectors.Vector :=
     [Definitions.An_Integer, Definitions.A_String];

   String_Or_Null : constant JSON_Schema.Simple_Type_Vectors.Vector :=
     [Definitions.A_String, Definitions.A_Null];

   Package_Name : constant VSS.Strings.Virtual_String := "DAP.Tools";

   ---------------------
   -- Escape_Keywords --
   ---------------------

   function Escape_Keywords (Name : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String
   is
      use type VSS.Strings.Virtual_String;
   begin
      if Name.Starts_With ("__") then
         declare
            Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
              Name.First_Character;
         begin
            if Cursor.Forward and then Cursor.Forward then
               return Name.Slice (Cursor, Name.Last_Character);
            else
               raise Program_Error;
            end if;
         end;
      elsif not Reserved_Words.Contains (Name) then
         declare
            --  Replace all spaces with underscores
            Words : constant VSS.String_Vectors.Virtual_String_Vector :=
              Name.Split (' ');
            Result : VSS.Strings.Virtual_String;
         begin
            for J in 1 .. Words.Length loop
               if J > 1 then
                  Result.Append ("_");
               end if;

               Result.Append (Words.Element (J));
            end loop;

            return Result;
         end;
      elsif Name.Starts_With ("i") or Name.Starts_With ("a") then
         return "an_" & Name;
      else
         return "a_" & Name;
      end if;
   end Escape_Keywords;

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Schema   : Schema_Access;
      Required : Boolean;
      Fallback : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      use type JSON_Schema.Simple_Type_Vectors.Vector;

      Result : VSS.Strings.Virtual_String :=
        (if Required then VSS.Strings.Empty_Virtual_String
         else "Optional_");
   begin
      if not Schema.Ref.Is_Empty then
         Result.Append (Ref_To_Type_Name (Schema.Ref));

         if Is_Enum (Map (Schema.Ref)) then
            Result.Prepend ("Enum.");
         end if;

      elsif not Schema.Additional_Properties.Is_Boolean
        and then Schema.Additional_Properties.Schema /= null
      then
         Result := "Any_Object";  --  TODO: Make more precise type???
      elsif Schema.Kind.Last_Index = 7 then
         Result.Append ("Any_Value");
      elsif Schema.Kind = Integer_Or_String then
         Result.Append ("Integer_Or_String");
      elsif Schema.Kind = String_Or_Null then
         --  To represent `null` it uses Is_Null function
         Result.Append ("VSS.Strings.Virtual_String");
      elsif Schema.Kind.Last_Index = 1 then
         case Schema.Kind (1) is
            when Definitions.A_Boolean =>
               --  Let's don't use Optional_Boolean because
               --  all absent boolean fields work as False
               Result := "Boolean";

            when Definitions.An_Integer =>
               Result.Append ("Integer");

            when Definitions.A_Null =>
               raise Program_Error;

            when Definitions.A_Number =>
               Result.Append ("Float");

            when Definitions.A_String =>

               if Required and Schema.Enum.Length = 1 then
                  --  If string type redefined as an enum with just one literal
                  --  then skip this property by returning an empty type name.
                  Result := VSS.Strings.Empty_Virtual_String;
               elsif Schema.Enum.Length > 1 then
                  Result.Prepend ("Enum.");
                  Result.Append (Fallback);
               else
                  --  Instead of Optional_String use just Virtual_String,
                  --  to check for an absent value use Is_Null.
                  Result := "VSS.Strings.Virtual_String";
               end if;

            when Definitions.An_Array =>
               declare
                  Item : constant Schema_Access :=
                    Schema.Items.First_Element;
               begin
                  --  TODO: Optional vectors???
                  if not Item.Ref.Is_Empty then
                     Result := Ref_To_Type_Name (Item.Ref);
                     Result.Append ("_Vector");
                  else
                     case Item.Kind (1) is
                        when Definitions.A_Boolean =>
                           Result := "Boolean_Vector";

                        when Definitions.An_Integer =>
                           Result := "Integer_Vector";

                        when Definitions.A_Null
                           | Definitions.An_Array
                           | Definitions.An_Object =>
                           raise Program_Error;

                        when Definitions.A_Number =>
                           Result := "Float_Vector";

                        when Definitions.A_String =>
                           Result :=
                             "VSS.String_Vectors.Virtual_String_Vector";
                     end case;
                  end if;
               end;

            when Definitions.An_Object =>
               Result.Append (Fallback);
         end case;
      else
         Result.Append ("YYY");
      end if;

      return Result;
   end Field_Type;

   ----------------------
   -- Find_Array_Types --
   ----------------------

   procedure Find_Array_Types
     (Map    : JSON_Schema.Readers.Schema_Map;
      Schema : Schema_Access;
      Result : in out Readers.Schema_Map)
   is
      use type Definitions.Simple_Types;

      Ref : VSS.Strings.Virtual_String;
   begin
      if Schema.Kind.Last_Index = 1
        and then Schema.Kind (1) = Definitions.An_Array
        and then not Schema.Items.First_Element.Ref.Is_Empty
      then
         pragma Assert (Schema.Items.Last_Index = 1);
         Ref := Schema.Items.First_Element.Ref;
         Result.Include (Ref_To_Type_Name (Ref), Map (Ref));
      end if;

      for Item of Schema.All_Of loop
         if Item.Ref.Is_Empty then
            Find_Array_Types (Map, Item, Result);
         end if;
      end loop;

      for Property of Schema.Properties loop
         Find_Array_Types (Map, Property.Schema, Result);
      end loop;
   end Find_Array_Types;

   -------------------------
   -- Find_Optional_Types --
   -------------------------

   procedure Find_Optional_Types
     (Schema : Schema_Access;
      Result : in out String_Sets.Set)
   is
      procedure Append_Optional (Schema : Schema_Access);

      ---------------------
      -- Append_Optional --
      ---------------------

      procedure Append_Optional (Schema : Schema_Access) is
      begin
         if not Schema.Ref.Is_Empty then
            Result.Include (Ref_To_Type_Name (Schema.Ref));
         end if;
      end Append_Optional;
   begin

      for Item of Schema.All_Of loop
         if Item.Ref.Is_Empty then
            Find_Optional_Types (Item, Result);
         end if;
      end loop;

      for Property of Schema.Properties loop
         if not Schema.Required.Contains (Property.Name) then
            Append_Optional (Property.Schema);
         end if;

         Find_Optional_Types (Property.Schema, Result);
      end loop;
   end Find_Optional_Types;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Ada.Wide_Wide_Text_IO.New_Line;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : VSS.Strings.Virtual_String) is
   begin
      Ada.Wide_Wide_Text_IO.Put
        (VSS.Strings.Conversions.To_Wide_Wide_String (Text));
   end Put;

   ---------------
   -- Type_Name --
   ---------------

   function Ref_To_Type_Name (Subschema : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String
   is
      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        Subschema.Split ('/');
   begin
      return List.Element (List.Length);
   end Ref_To_Type_Name;

   -----------
   -- Write --
   -----------

   procedure Write (Map : JSON_Schema.Readers.Schema_Map) is
   begin
      Write_Type_Package (Map);
   end Write;

   --------------------------
   -- Write_Anonymous_Type --
   --------------------------

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property_Name  : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set;
      Required       : Boolean)
   is
      use type VSS.Strings.Virtual_String;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Enclosing_Type)
        & "_" & Property_Name;
   begin
      --  Write dependencies
      for Property of Schema.Properties loop
         if not Property.Schema.Ref.Is_Empty then
            Write_Named_Type
              (Property.Schema.Ref,
               Map (Property.Schema.Ref),
               Map,
               Optional_Types,
               Done);
         end if;
      end loop;

      Put ("type ");
      Put (Type_Name);
      Put (" is ");
      Put ("record");
      New_Line;

      for Property of Schema.Properties loop
         Write_Record_Component
           (Enclosing_Type,
            Map,
            Property,
            Schema.Required.Contains (Property.Name));
      end loop;

      Put ("end record;");
      New_Line;
      New_Line;

      if not Required then
         Write_Optional_Type (Type_Name);
      end if;
   end Write_Anonymous_Type;

   ----------------------
   -- Write_Any_Object --
   ----------------------

   procedure Write_Any_Object (Name : VSS.Strings.Virtual_String) is
   begin
      Put ("type ");
      Put (Ref_To_Type_Name (Name));
      Put (" is new Any_Object with null record;");
      New_Line;
   end Write_Any_Object;

   -------------------
   -- Write_Comment --
   -------------------

   procedure Write_Comment
     (Description : VSS.Strings.Virtual_String;
      Indent      : Natural) is
   begin
      if not Description.Is_Empty then
         declare
            Lines : constant VSS.String_Vectors.Virtual_String_Vector :=
              Description.Split_Lines;
         begin
            for Line in 1 .. Lines.Length loop
               for J in 1 .. Indent loop
                  Put (" ");
               end loop;

               Put ("--  ");
               Put (Lines (Line));
               New_Line;
            end loop;
         end;
      end if;
   end Write_Comment;

   ------------------------
   -- Write_Derived_Type --
   ------------------------

   procedure Write_Derived_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set)
   is
      use type VSS.Strings.Virtual_String;

      Next : Schema_Access := Schema;

      Done_Fields : String_Sets.Set;
   begin
      --  Write dependencies
      for Used of Schema.All_Of loop
         if not Used.Ref.Is_Empty then
            Write_Named_Type
              (Used.Ref,
               Map (Used.Ref),
               Map,
               Optional_Types,
               Done);
         end if;

         for Property of Used.Properties loop
            if not Property.Schema.Ref.Is_Empty
              and then Property.Schema.Ref /= Name
            then
               Write_Named_Type
                 (Property.Schema.Ref,
                  Map (Property.Schema.Ref),
                  Map,
                  Optional_Types,
                  Done);
            elsif Property.Schema.Kind.Last_Index = 1 then
               case Property.Schema.Kind (1) is
                  when Definitions.An_Object =>
                     Write_Anonymous_Type
                       (Name,
                        Property.Name,
                        Property.Schema,
                        Map,
                        Optional_Types,
                        Done,
                        Property.Schema.Required.Contains (Property.Name));
                  when others =>
                     null;
               end case;
            end if;
         end loop;
      end loop;

      pragma Assert (Schema.All_Of.Last_Index = 2);
      Put ("type ");
      Put (Ref_To_Type_Name (Name));
      Put (" is ");

      Put ("record");
      New_Line;

      while Next /= null loop
         for Property of Next.Properties loop
            if not Done_Fields.Contains (Property.Name) then
               Write_Record_Component
                 (Name, Map, Property, Next.Required.Contains (Property.Name));
               Done_Fields.Include (Property.Name);
            end if;
         end loop;

         for Item of Next.All_Of loop
            for Property of Item.Properties loop
               if not Done_Fields.Contains (Property.Name) then
                  Write_Record_Component
                    (Name,
                     Map,
                     Property,
                     Item.Required.Contains (Property.Name));

                  Done_Fields.Include (Property.Name);
               end if;
            end loop;
         end loop;

         if Next.All_Of.Is_Empty then
            Next := null;
         else
            Next := Map (Next.All_Of.First_Element.Ref);
         end if;
      end loop;

      Put ("end record;");
      New_Line;
      New_Line;
   end Write_Derived_Type;

   ----------------------------
   -- Write_Enumeration_Type --
   ----------------------------

   procedure Write_Enumeration_Type
     (Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access)
   is
   begin
      Put ("type ");
      Put (Name);

      Put (" is (");

      for Index in 1 .. Schema.Enum.Length loop
         if Index > 1 then
            Put (", ");
         end if;

         Put (Escape_Keywords (Schema.Enum.Element (Index)));
      end loop;

      Put (");");
      New_Line;
      New_Line;
   end Write_Enumeration_Type;

   -----------------------------
   -- Write_Enumeration_Types --
   -----------------------------

   procedure Write_Enumeration_Types
     (Name     : VSS.Strings.Virtual_String;
      Property : VSS.Strings.Virtual_String;
      Schema   : Schema_Access;
      Required : Boolean)
   is
   begin
      if Schema.Enum.Length > 1 then
         declare
            Type_Name : VSS.Strings.Virtual_String := Name;
         begin
            if not Property.Is_Empty then
               Type_Name.Append ("_");
               Type_Name.Append (Property);
            end if;

            Write_Enumeration_Type (Type_Name, Schema);

            if not Required then
               Write_Optional_Type (Type_Name);
            end if;
         end;
      end if;

      for Item of Schema.All_Of loop
         if Item.Ref.Is_Empty then
            Write_Enumeration_Types (Name, Property, Item, True);
         end if;
      end loop;

      for Property of Schema.Properties loop
         Write_Enumeration_Types
           (Name,
            Property.Name,
            Property.Schema,
            Schema.Required.Contains (Property.Name));
      end loop;
   end Write_Enumeration_Types;

   ----------------------
   -- Write_Named_Type --
   ----------------------

   procedure Write_Named_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set)
   is
      use type Definitions.Simple_Types;
   begin
      if Done.Contains (Name) then
         return;
      elsif not Schema.Enum.Is_Empty then
         Done.Insert (Name);
         Write_Enumeration_Type (Ref_To_Type_Name (Name), Schema);
      elsif not Schema.All_Of.Is_Empty then
         Done.Insert (Name);
         Write_Derived_Type
           (Name, Schema, Map, Optional_Types, Done);
      elsif not Schema.Properties.Is_Empty then
         Done.Insert (Name);
         Write_Record_Type (Name, Schema, Map, Optional_Types, Done);
      elsif Schema.Kind.Last_Index = 1
        and then Schema.Kind.First_Element = Definitions.An_Object
      then
         Done.Insert (Name);
         Write_Any_Object (Name);
      else
         Put ("--  Left:");
         Put (Name);
         New_Line;
      end if;

      if Optional_Types.Contains (Ref_To_Type_Name (Name)) then
         Write_Optional_Type (Name);
      end if;
   end Write_Named_Type;

   -----------------------
   -- Write_Named_Types --
   -----------------------

   procedure Write_Named_Types
     (Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set) is
   begin
      for Cursor in Map.Iterate loop
         declare
            Name : constant VSS.Strings.Virtual_String :=
              JSON_Schema.Readers.Schema_Maps.Key (Cursor);
            Schema : constant Schema_Access :=
              JSON_Schema.Readers.Schema_Maps.Element (Cursor);
         begin
            Write_Named_Type (Name, Schema, Map, Optional_Types, Done);
         end;
      end loop;
   end Write_Named_Types;

   -------------------------
   -- Write_Optional_Type --
   -------------------------

   procedure Write_Optional_Type (Name : VSS.Strings.Virtual_String) is
      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name);
   begin
      Put ("type Optional_");
      Put (Type_Name);
      Put (" (Is_Set : Boolean := False) is record");
      New_Line;
      Put ("case Is_Set is");
      New_Line;
      Put ("when True => Value : ");
      Put (Type_Name);
      Put (";");
      New_Line;
      Put ("when False => null;");
      New_Line;
      Put ("end case; end record;");
      New_Line;
      New_Line;
   end Write_Optional_Type;

   ---------------------------
   -- Write_Private_Vectors --
   ---------------------------

   procedure Write_Private_Vectors (Array_Types : Readers.Schema_Map) is
   begin
      for Cursor in Array_Types.Iterate loop
         declare
            use type VSS.Strings.Virtual_String;

            Item : constant VSS.Strings.Virtual_String :=
              Readers.Schema_Maps.Key (Cursor);
            Schema : constant Schema_Access :=
              Readers.Schema_Maps.Element (Cursor);
            Element_Type : constant VSS.Strings.Virtual_String :=
              (if Is_Enum (Schema) then "Enum." & Item else Item);
         begin
            Put ("type ");
            Put (Item);
            Put ("_Array is array (Positive range <>) of aliased ");
            Put (Element_Type);
            Put (";");
            New_Line;
            Put ("type ");
            Put (Item);
            Put ("_Array_Access is access ");
            Put (Item);
            Put ("_Array;");
            New_Line;
            Put ("type ");
            Put (Item);
            Put ("_Vector is new Ada.Finalization.Controlled with record");
            New_Line;
            Put ("Data   : ");
            Put (Item);
            Put ("_Array_Access;");
            New_Line;
            Put ("Length : Natural := 0;");
            New_Line;
            Put ("end record;");
            New_Line;
            New_Line;

            Put ("overriding procedure Adjust (Self : in out ");
            Put (Item);
            Put ("_Vector);");
            New_Line;
            New_Line;

            Put ("overriding procedure Finalize (Self : in out ");
            Put (Item);
            Put ("_Vector);");
            New_Line;
            New_Line;

         end;
      end loop;
   end Write_Private_Vectors;

   --------------------------
   -- Write_Public_Vectors --
   --------------------------

   procedure Write_Public_Vectors (Array_Types : Readers.Schema_Map) is
   begin
      for Cursor in Array_Types.Iterate loop
         declare
            Item : constant VSS.Strings.Virtual_String :=
              Readers.Schema_Maps.Key (Cursor);
         begin
            Put ("type ");
            Put (Item);
            Put ("_Vector is tagged private");
            New_Line;
            Put ("with Variable_Indexing => Get_");
            Put (Item);
            Put ("_Variable_Reference,");
            New_Line;
            Put ("Constant_Indexing => Get_");
            Put (Item);
            Put ("_Constant_Reference;");
            New_Line;
            New_Line;
         end;
      end loop;
   end Write_Public_Vectors;

   ----------------------------
   -- Write_Record_Component --
   ----------------------------

   procedure Write_Record_Component
     (Name     : VSS.Strings.Virtual_String;
      Map      : JSON_Schema.Readers.Schema_Map;
      Property : JSON_Schema.Property;
      Required : Boolean)
   is
      use type VSS.Strings.Virtual_String;

      Fallback : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name) & "_" & Property.Name;
   begin
      declare
         Field_Name : constant VSS.Strings.Virtual_String :=
           Escape_Keywords (Property.Name);

         Field_Type : VSS.Strings.Virtual_String :=
           Writers.Field_Type (Map, Property.Schema, Required, Fallback);
      begin
         if Field_Type.Is_Empty then
            --  Skip unneeded properties
            return;
         elsif Field_Name.To_Lowercase = Field_Type.To_Lowercase then
            Field_Type.Prepend (".");
            Field_Type.Prepend (Package_Name);
         end if;

         Put (Field_Name);
         Put (" : ");
         Put (Field_Type);
         Put (";");
         New_Line;

         Write_Comment (Property.Schema.Description, 6);
      end;
   end Write_Record_Component;

   -----------------------
   -- Write_Record_Type --
   -----------------------

   procedure Write_Record_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set)
   is
      use type VSS.Strings.Virtual_String;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name);
   begin
      --  Write dependencies
      for Property of Schema.Properties loop
         if not Property.Schema.Ref.Is_Empty
           and then Property.Schema.Ref /= Name
         then
            Write_Named_Type
              (Property.Schema.Ref,
               Map (Property.Schema.Ref),
               Map,
               Optional_Types,
               Done);
         end if;
      end loop;

      Put ("type ");
      Put (Type_Name);
      Put (" is ");
      Put ("record");
      New_Line;

      for Property of Schema.Properties loop
         Write_Record_Component
           (Name,
            Map,
            Property,
            Schema.Required.Contains (Property.Name));
      end loop;

      Put ("end record;");
      New_Line;
      New_Line;
   end Write_Record_Type;

   ------------------------
   -- Write_Type_Package --
   ------------------------

   procedure Write_Type_Package (Map : JSON_Schema.Readers.Schema_Map) is
      Optional_Types : String_Sets.Set;
      Array_Types    : Readers.Schema_Map;
      Done           : String_Sets.Set;
   begin
      for Schema of Map loop
         Find_Optional_Types (Schema, Optional_Types);
         Find_Array_Types (Map, Schema, Array_Types);
      end loop;

      Put ("with Ada.Finalization;");
      New_Line;
      Put ("with VSS.Strings;");
      New_Line;
      Put ("with VSS.String_Vectors;");
      New_Line;

      Put ("package ");
      Put (Package_Name);
      Put (" is");
      New_Line;
      Put ("type Any_Object is tagged null record;");
      New_Line;
      Put ("type Any_Value is null record;");
      Write_Optional_Type ("Any_Value");
      Write_Optional_Type ("Integer");
      Write_Optional_Type ("Float");
      Put ("type Integer_Or_String is null record;");
      New_Line;
      Write_Optional_Type ("Integer_Or_String");
      New_Line;
      Put ("type Integer_Vector is tagged private;");
      New_Line;

      Write_Public_Vectors (Array_Types);
      New_Line;

      --  Write all enumenration types. Use a nested package to avoid
      --  name colisions between enumeration literals and types.
      Put ("package Enum is");
      New_Line;
      New_Line;

      for Cursor in Map.Iterate loop
         declare
            Name : constant VSS.Strings.Virtual_String :=
              JSON_Schema.Readers.Schema_Maps.Key (Cursor);

            Schema : constant Schema_Access :=
              JSON_Schema.Readers.Schema_Maps.Element (Cursor);

            Type_Name : constant VSS.Strings.Virtual_String :=
              Ref_To_Type_Name (Name);
         begin
            Write_Enumeration_Types
              (Type_Name,
               "",
               Schema,
               not Optional_Types.Contains (Type_Name));

            if not Schema.Enum.Is_Empty then
               Done.Insert (Name);
            end if;
         end;
      end loop;

      Put ("end Enum;");
      New_Line;
      New_Line;

      Write_Named_Types (Map, Optional_Types, Done);

      Write_Vector_Operations (Array_Types, Specification);
      Put ("private");
      New_Line;
      Put ("type Integer_Vector is tagged null record;");
      New_Line;
      Write_Private_Vectors (Array_Types);
      Put ("end ");
      Put (Package_Name);
      Put (";");
      New_Line;

      Put ("with Ada.Unchecked_Deallocation;");
      New_Line;
      New_Line;
      Put ("package body ");
      Put (Package_Name);
      Put (" is");
      New_Line;
      Write_Vector_Operations (Array_Types, Implemenetation);
      Put ("end ");
      Put (Package_Name);
      Put (";");
   end Write_Type_Package;

   -----------------------------
   -- Write_Vector_Operations --
   -----------------------------

   procedure Write_Vector_Operations
     (Array_Types : Readers.Schema_Map;
      Kind        : Declaration_Kind)
   is
      use type VSS.Strings.Virtual_String;
   begin
      for Cursor in Array_Types.Iterate loop
         declare
            Item : constant VSS.Strings.Virtual_String :=
              Readers.Schema_Maps.Key (Cursor);
            Schema : constant Schema_Access :=
              Readers.Schema_Maps.Element (Cursor);
            Element_Type : constant VSS.Strings.Virtual_String :=
              (if Is_Enum (Schema) then "Enum." & Item else Item);
         begin

            if Kind = Implemenetation then
               Put ("procedure Free is new Ada.Unchecked_Deallocation");
               New_Line;
               Put ("(");
               Put (Item);
               Put ("_Array, ");
               Put (Item);
               Put ("_Array_Access);");
               New_Line;
               New_Line;

               Put ("overriding procedure Adjust (Self : in out ");
               Put (Item);
               Put ("_Vector) is");
               New_Line;
               Put ("begin");
               New_Line;
               Put ("if Self.Length > 0 then");
               New_Line;
               Put ("Self.Data :=");
               New_Line;
               Put ("new ");
               Put (Item);
               Put ("_Array'(Self.Data (1 .. Self.Length));");
               New_Line;
               Put ("end if;");
               New_Line;
               Put ("end Adjust;");

               New_Line;
               New_Line;

               Put ("overriding procedure Finalize (Self : in out ");
               Put (Item);
               Put ("_Vector) is");
               New_Line;
               Put ("begin");
               New_Line;
               Put ("Free (Self.Data);");
               New_Line;
               Put ("Self.Length := 0;");
               New_Line;
               Put ("end Finalize;");
               New_Line;
               New_Line;
            end if;

            Put ("function Length (Self : ");
            Put (Item);
            Put ("_Vector) return Natural");

            if Kind = Implemenetation then
               Put (" is (Self.Length)");
            end if;

            Put (";");
            New_Line;
            New_Line;

            Put ("procedure Clear (Self : in out ");
            Put (Item);
            Put ("_Vector)");

            if Kind = Implemenetation then
               Put (" is");
               New_Line;
               Put ("begin");
               New_Line;
               Put ("   Self.Length := 0;");
               New_Line;
               Put ("end Clear");
            end if;

            Put (";");
            New_Line;
            New_Line;
            Put ("procedure Append");
            New_Line;
            Put ("(Self : in out ");
            Put (Item);
            Put ("_Vector;");
            New_Line;
            Put ("Value : ");
            Put (Element_Type);
            Put (")");

            if Kind = Implemenetation then
               Put (" is");
               New_Line;
               Put ("Init_Length : constant Positive :=");
               New_Line;

               Put ("Positive'Max (1, 256 / ");
               Put (Element_Type);
               Put ("'Size);");
               New_Line;
               Put ("Self_Data_Saved : ");
               Put (Item);
               Put ("_Array_Access := Self.Data;");
               New_Line;
               Put ("begin");
               New_Line;
               Put ("if Self.Length = 0 then");
               New_Line;
               Put ("Self.Data := new ");
               Put (Item);
               Put ("_Array (1 .. Init_Length);");
               New_Line;
               Put ("elsif Self.Length = Self.Data'Last then");
               New_Line;
               Put ("Self.Data :=");
               New_Line;
               Put ("new ");
               Put (Item);
               Put ("_Array'");
               Put ("(Self.Data.all");
               New_Line;
               Put (" & ");
               Put (Item);
               Put ("_Array'(1 .. Self.Length => <>));");
               New_Line;
               Put ("Free (Self_Data_Saved);");
               New_Line;
               Put ("end if;");
               New_Line;
               Put ("Self.Length := Self.Length + 1;");
               New_Line;
               Put ("Self.Data (Self.Length) := Value;");
               New_Line;
               Put ("end Append");
            end if;

            Put (";");
            New_Line;
            New_Line;

            if Kind = Specification then
               Put ("type ");
               Put (Item);
               Put ("_Variable_Reference");
               New_Line;
               Put ("(Element : not null access ");
               Put (Element_Type);
               Put (") is null record");
               New_Line;
               Put ("with Implicit_Dereference => Element;");
               New_Line;
               New_Line;
            end if;

            Put ("not overriding function Get_");
            Put (Item);
            Put ("_Variable_Reference");
            New_Line;
            Put ("(Self : aliased in out ");
            Put (Item);
            Put ("_Vector;");
            New_Line;
            Put ("Index : Positive)");
            New_Line;
            Put ("return ");
            Put (Item);
            Put ("_Variable_Reference");
            New_Line;

            if Kind = Specification then
               Put ("with Inline;");
            else
               Put ("is (Element => Self.Data (Index)'Access);");
            end if;

            New_Line;
            New_Line;

            if Kind = Specification then
               Put ("type ");
               Put (Item);
               Put ("_Constant_Reference");
               New_Line;
               Put ("(Element : not null access constant ");
               Put (Element_Type);
               Put (") is null record");
               New_Line;
               Put ("with Implicit_Dereference => Element;");
               New_Line;
               New_Line;
            end if;

            Put ("not overriding function Get_");
            Put (Item);
            Put ("_Constant_Reference");
            New_Line;
            Put ("(Self : aliased in out ");
            Put (Item);
            Put ("_Vector;");
            New_Line;
            Put ("Index : Positive)");
            New_Line;
            Put ("return ");
            Put (Item);
            Put ("_Constant_Reference");
            New_Line;

            if Kind = Specification then
               Put ("with Inline;");
            else
               Put ("is (Element => Self.Data (Index)'Access);");
            end if;

            New_Line;
            New_Line;
         end;
      end loop;
   end Write_Vector_Operations;

end JSON_Schema.Writers;
