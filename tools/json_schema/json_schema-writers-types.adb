--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with JSON_Schema.Writers.Inputs;
with JSON_Schema.Writers.Outputs;

package body JSON_Schema.Writers.Types is

   procedure Write_Named_Types
     (Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  Generate types for all named schemas in Map if they not present in Done
   --  already. Include names of generated type in Done set.

   procedure Write_Named_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  Generate a type for single named Schema. Generate Optional_ type if
   --  corresponding type is included in Optional_Types set. Update Done as
   --  described above.

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set;
      Required       : Boolean;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector);
   --  Write a dedicated type for a schema property that contains a nested
   --  schema. Currently only single level of nesting is implemented.

   procedure Write_Record_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  The same for "type: object" schema

   procedure Write_Record_Component
     (Name         : VSS.Strings.Virtual_String;
      Map          : JSON_Schema.Readers.Schema_Map;
      Root_Package : VSS.Strings.Virtual_String;
      Property     : JSON_Schema.Property;
      Required     : Boolean;
      Is_Holder    : Boolean);
   --  Write record component declaration for given Property. Name is a name of
   --  the schema containing the property.

   procedure Write_Derived_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  The same for "allOf:[]" schema

   procedure Write_Union_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  The same for "anyOf:[]" schema

   procedure Write_Enumeration_Type
     (Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access);
   --  The same for enumeration schema

   procedure Write_Union_Variant_Type
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access);
   --  Write enumeration type for variant in anyOf schema

   procedure Write_Public_Vectors (Array_Types : Readers.Schema_Map);
   --  Write vector type public declarations for each item of Array_Types

   procedure Write_Vector_Operations
     (Array_Types : Readers.Schema_Map;
      Kind        : Declaration_Kind);
   --  Write vector type operations for each item of Array_Types

   procedure Write_Private_Vectors
    (Array_Types  : Readers.Schema_Map;
     Enum_Package : VSS.Strings.Virtual_String);
   --  Write vector type private declarations for each item of Array_Types

   procedure Write_Public_Holders
     (Map     : JSON_Schema.Readers.Schema_Map;
      Holders : VSS.String_Vectors.Virtual_String_Vector);
   --  Write holder types

   procedure Write_Holders_Operations
     (Map     : JSON_Schema.Readers.Schema_Map;
      Holders : VSS.String_Vectors.Virtual_String_Vector;
      Kind    : Declaration_Kind);

   procedure Write_Privete_Holders
     (Map     : JSON_Schema.Readers.Schema_Map;
      Holders : VSS.String_Vectors.Virtual_String_Vector);
   --  Write holder private (full) types

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

   procedure Write_Comment
     (Description : VSS.Strings.Virtual_String;
      Indent      : Natural);
   --  Write Description as an Ada comment (handle LF if any).
   --  Use Indent spaces before comment markers.

   procedure Write_Optional_Type (Name : VSS.Strings.Virtual_String);
   --  Write an Optional_<Name> type for given type Name

   procedure Write_Any_Object (Name : VSS.Strings.Virtual_String);
   --  Write an Ada type that represents an unrestricted object

   procedure Write_Type_Package
     (Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Header         : VSS.String_Vectors.Virtual_String_Vector;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Array_Types    : Readers.Schema_Map);
   --  Write package specification with type declarations

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

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Schema   : Schema_Access;
      Required : Boolean;
      Fallback : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      Name : VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String;

   begin
      Get_Field_Type (Map, Schema, Required, Fallback, Name, Prefix);
      Prefix.Append (Name);

      return Prefix;
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

   procedure Write
     (Map          : JSON_Schema.Readers.Schema_Map;
      Root_Package : VSS.Strings.Virtual_String;
      Enum_Package : VSS.Strings.Virtual_String;
      Header       : VSS.String_Vectors.Virtual_String_Vector;
      Holders      : VSS.String_Vectors.Virtual_String_Vector)
   is
      Optional_Types : String_Sets.Set;
      Array_Types    : Readers.Schema_Map;
   begin
      for Schema of Map loop
         Find_Optional_Types (Schema, Optional_Types);
         Find_Array_Types (Map, Schema, Array_Types);
      end loop;

      Array_Types.Insert ("Integer", null);

      Write_Type_Package
        (Map, Root_Package, Enum_Package, Header, Holders,
         Optional_Types, Array_Types);

      JSON_Schema.Writers.Outputs.Generate_Writers
        (Map, Root_Package, Enum_Package, Header, Holders, Optional_Types);

      JSON_Schema.Writers.Inputs.Generate_Readers
        (Map, Root_Package, Enum_Package, Header, Holders, Optional_Types);
   end Write;

   --------------------------
   -- Write_Anonymous_Type --
   --------------------------

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set;
      Required       : Boolean;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Enclosing_Type)
        & "_" & Property.Name;
   begin
      --  Write dependencies
      for Item of Property.Schema.Properties loop
         if not Item.Schema.Ref.Is_Empty then
            Write_Named_Type
              (Item.Schema.Ref,
               Map (Item.Schema.Ref),
               Map,
               Root_Package,
               Enum_Package,
               Holders,
               Optional_Types,
               Done);
         end if;
      end loop;

      Put ("type ");
      Put (Type_Name);
      Put (" is ");
      Put ("record");
      New_Line;

      for Item of Property.Schema.Properties loop
         Write_Record_Component
           (Enclosing_Type,
            Map,
            Root_Package,
            Item,
            Property.Schema.Required.Contains (Item.Name),
            False);
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
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set)
   is
      use type VSS.Strings.Virtual_String;

      procedure On_Property
        (Property : JSON_Schema.Property;
         Required : Boolean);
      --  Generate component declaration for given property

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property);
      --  Generate anonymous type for given property

      -------------------------
      -- On_Anonymous_Schema --
      -------------------------

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type
           (Name,
            Property,
            Map,
            Optional_Types,
            Done,
            Property.Schema.Required.Contains (Property.Name),
            Root_Package, Enum_Package, Holders);
      end On_Anonymous_Schema;

      -----------------
      -- On_Property --
      -----------------

      procedure On_Property
        (Property : JSON_Schema.Property;
         Required : Boolean) is
      begin
         Write_Record_Component
           (Name, Map, Root_Package, Property, Required, False);
      end On_Property;

   begin
      --  Write dependencies
      for Used of Schema.All_Of loop
         if not Used.Ref.Is_Empty then
            Write_Named_Type
              (Used.Ref,
               Map (Used.Ref),
               Map,
               Root_Package,
               Enum_Package,
               Holders,
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
                  Root_Package,
                  Enum_Package,
                  Holders,
                  Optional_Types,
                  Done);
            end if;
         end loop;
      end loop;

      Each_Anonymous_Schema (Map, Schema, On_Anonymous_Schema'Access);

      pragma Assert (Schema.All_Of.Last_Index = 2);
      Put ("type ");
      Put (Ref_To_Type_Name (Name));
      Put (" is ");

      Put ("record");
      New_Line;

      Writers.Each_Property (Map, Schema, On_Property'Access);

      Put ("end record;");
      New_Line;
      New_Line;
   end Write_Derived_Type;

   ----------------------------
   -- Write_Enumeration_Type --
   ----------------------------

   procedure Write_Enumeration_Type
     (Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access) is
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

   ------------------------------
   -- Write_Holders_Operations --
   ------------------------------

   procedure Write_Holders_Operations
     (Map     : JSON_Schema.Readers.Schema_Map;
      Holders : VSS.String_Vectors.Virtual_String_Vector;
      Kind    : Declaration_Kind)
   is
      procedure Each (Name : VSS.Strings.Virtual_String);

      procedure Each (Name : VSS.Strings.Virtual_String) is
         Type_Name : constant VSS.Strings.Virtual_String :=
           Ref_To_Type_Name (Name);
      begin
         Put ("not overriding function Element"); New_Line;
         Put ("(Self  : aliased in out ");
         Put (Type_Name);
         Put ("_Holder)"); New_Line;
         Put (" return ");
         Put (Type_Name);
         Put ("_Variable_Reference");

         if Kind = Specification then
            Put (" with Inline;");
            New_Line;
            New_Line;
         else
            Put (" is"); New_Line;
            Put ("begin"); New_Line;
            Put ("if Self.Length = 0 then"); New_Line;
            Put ("Self.Append ((others => <>));"); New_Line;
            Put ("end if;"); New_Line;
            Put ("return (Element => Self.Data (1)'Access);"); New_Line;
            Put ("end Element;"); New_Line;
            New_Line;
         end if;

         Put ("not overriding function Value"); New_Line;
         Put ("(Self  : aliased ");
         Put (Type_Name);
         Put ("_Holder)"); New_Line;
         Put (" return ");
         Put (Type_Name);
         Put ("_Constant_Reference");

         if Kind = Specification then
            Put (" with Inline;");
            New_Line;
            New_Line;
         else
            Put (" is (Element => Self.Data (1)'Access);");
            New_Line;
            New_Line;
         end if;
      end Each;
   begin
      Each_Holder_Type (Map, Holders, Each'Access);
   end Write_Holders_Operations;

   ----------------------
   -- Write_Named_Type --
   ----------------------

   procedure Write_Named_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
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
      elsif not Schema.Any_Of.Is_Empty then
         Done.Insert (Name);
         Write_Union_Type
           (Name, Schema, Map, Root_Package, Enum_Package, Holders,
            Optional_Types, Done);
      elsif not Schema.All_Of.Is_Empty then
         Done.Insert (Name);
         Write_Derived_Type
           (Name, Schema, Map, Root_Package, Enum_Package, Holders,
            Optional_Types, Done);
      elsif not Schema.Properties.Is_Empty then
         Done.Insert (Name);
         Write_Record_Type
           (Name, Schema, Map, Root_Package, Enum_Package, Holders,
            Optional_Types, Done);
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
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
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
            Write_Named_Type
              (Name, Schema, Map, Root_Package, Enum_Package, Holders,
               Optional_Types, Done);
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

   procedure Write_Private_Vectors
    (Array_Types  : Readers.Schema_Map;
     Enum_Package : VSS.Strings.Virtual_String) is
   begin
      for Cursor in Array_Types.Iterate loop
         declare
            use type VSS.Strings.Virtual_String;

            Item : constant VSS.Strings.Virtual_String :=
              Readers.Schema_Maps.Key (Cursor);
            Schema : constant Schema_Access :=
              Readers.Schema_Maps.Element (Cursor);
            Element_Type : constant VSS.Strings.Virtual_String :=
              (if Schema /= null
                 and then Is_Enum (Schema)
                 and then not Enum_Package.Is_Empty
               then Enum_Package & "." & Item
               else Item);
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

   ---------------------------
   -- Write_Privete_Holders --
   ---------------------------

   procedure Write_Privete_Holders
     (Map     : JSON_Schema.Readers.Schema_Map;
      Holders : VSS.String_Vectors.Virtual_String_Vector)
   is
      procedure Each (Name : VSS.Strings.Virtual_String);

      procedure Each (Name : VSS.Strings.Virtual_String) is
         Type_Name : constant VSS.Strings.Virtual_String :=
           Ref_To_Type_Name (Name);
      begin
         Put ("type ");
         Put (Type_Name);
         Put ("_Holder is new ");
         Put (Type_Name);
         Put ("_Vector with null record;");
         New_Line;
         New_Line;
      end Each;
   begin
      Each_Holder_Type (Map, Holders, Each'Access);
   end Write_Privete_Holders;

   --------------------------
   -- Write_Public_Holders --
   --------------------------

   procedure Write_Public_Holders
     (Map     : JSON_Schema.Readers.Schema_Map;
      Holders : VSS.String_Vectors.Virtual_String_Vector)
   is
      procedure Each (Name : VSS.Strings.Virtual_String);

      procedure Each (Name : VSS.Strings.Virtual_String) is
         Type_Name : constant VSS.Strings.Virtual_String :=
           Ref_To_Type_Name (Name);
      begin
         Put ("type ");
         Put (Type_Name);
         Put ("_Holder is tagged private;");
         New_Line;
         New_Line;
      end Each;
   begin
      Each_Holder_Type (Map, Holders, Each'Access);
   end Write_Public_Holders;

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
     (Name         : VSS.Strings.Virtual_String;
      Map          : JSON_Schema.Readers.Schema_Map;
      Root_Package : VSS.Strings.Virtual_String;
      Property     : JSON_Schema.Property;
      Required     : Boolean;
      Is_Holder    : Boolean)
   is
      use type VSS.Strings.Virtual_String;

      Fallback : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name) & "_" & Property.Name;
   begin
      declare
         Field_Name : constant VSS.Strings.Virtual_String :=
           Escape_Keywords (Property.Name);

         Field_Type : VSS.Strings.Virtual_String :=
           Writers.Types.Field_Type (Map, Property.Schema, Required, Fallback);
      begin
         if Field_Type.Is_Empty then
            --  Skip unneeded properties
            return;
         elsif Is_Holder then
            Field_Type.Append ("_Holder");
         elsif Field_Name.To_Lowercase = Field_Type.To_Lowercase then
            Field_Type.Prepend (".");
            Field_Type.Prepend (Root_Package);
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
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
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
            if not Is_Holder_Field (Name, Property.Name, Holders) then
               Write_Named_Type
                 (Property.Schema.Ref,
                  Map (Property.Schema.Ref),
                  Map,
                  Root_Package,
                  Enum_Package,
                  Holders,
                  Optional_Types,
                  Done);
            end if;
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
            Root_Package,
            Property,
            Schema.Required.Contains (Property.Name),
            Is_Holder_Field (Name, Property.Name, Holders));
      end loop;

      Put ("end record;");
      New_Line;
      New_Line;
   end Write_Record_Type;

   ------------------------
   -- Write_Type_Package --
   ------------------------

   procedure Write_Type_Package
     (Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Header         : VSS.String_Vectors.Virtual_String_Vector;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Array_Types    : Readers.Schema_Map)
   is
      Done : String_Sets.Set;
   begin
      for Item of Header loop
         Put (Item);
         New_Line;
      end loop;

      Put ("with Ada.Containers.Doubly_Linked_Lists;");
      New_Line;
      Put ("with Ada.Finalization;");
      New_Line;
      Put ("with VSS.JSON.Events;");
      New_Line;
      Put ("with VSS.Strings;");
      New_Line;
      Put ("with VSS.String_Vectors;");
      New_Line;
      New_Line;

      Put ("package ");
      Put (Root_Package);
      Put (" is");
      New_Line;
      Put
        ("package JSON_Event_Lists is new Ada.Containers.Doubly_Linked_Lists");
      New_Line;
      Put ("(VSS.JSON.Events.JSON_Event, VSS.JSON.Events.""="");");
      New_Line;
      New_Line;
      Put ("type Any_Value is new JSON_Event_Lists.List with null record;");
      New_Line;
      Put ("type Any_Object is new Any_Value with null record;");
      New_Line;
      New_Line;
      Write_Optional_Type ("Integer");
      Write_Optional_Type ("Float");
      Put ("type Integer_Or_String (Is_String : Boolean := False) is record");
      New_Line;
      Put ("case Is_String is");
      New_Line;
      Put ("when False =>");
      New_Line;
      Put ("Integer : Standard.Integer;");
      New_Line;
      Put ("when True =>");
      New_Line;
      Put ("String : VSS.Strings.Virtual_String;");
      New_Line;
      Put ("end case;");
      New_Line;
      Put ("end record;");
      New_Line;
      New_Line;
      Write_Optional_Type ("Integer_Or_String");
      New_Line;

      Write_Public_Vectors (Array_Types);
      Write_Public_Holders (Map, Holders);
      New_Line;

      --  Write all enumeration types. Use a nested package to avoid
      --  name collisions between enumeration literals and types.
      if not Enum_Package.Is_Empty then
         Put ("package ");
         Put (Enum_Package);
         Put (" is");
         New_Line;
         New_Line;
      end if;

      declare
         procedure On_Enumeration_Type
           (Name     : VSS.Strings.Virtual_String;
            Property : VSS.Strings.Virtual_String;
            Schema   : Schema_Access;
            Optional : Boolean);
         --  Generate enumeration type for Schema

         procedure On_Enumeration_Type
           (Name     : VSS.Strings.Virtual_String;
            Property : VSS.Strings.Virtual_String;
            Schema   : Schema_Access;
            Optional : Boolean)
         is
            Type_Name : VSS.Strings.Virtual_String := Ref_To_Type_Name (Name);
         begin
            if Property.Is_Empty then
               Done.Include (Name);
            end if;

            if Schema.Enum.Length > 1 then
               if not Property.Is_Empty then
                  Type_Name.Append ("_");
                  Type_Name.Append (Property);
               end if;

               Write_Enumeration_Type (Type_Name, Schema);

               if Optional then
                  Write_Optional_Type (Type_Name);
               end if;
            end if;
         end On_Enumeration_Type;
      begin
         Each_Enumeration_Type
           (Map, Optional_Types, On_Enumeration_Type'Access);
      end;

      declare
         procedure On_Union_Type
           (Name     : VSS.Strings.Virtual_String;
            Property : VSS.Strings.Virtual_String;
            Schema   : Schema_Access;
            Optional : Boolean);
         --  Generate enumeration type for anyOf Schema

         procedure On_Union_Type
           (Name     : VSS.Strings.Virtual_String;
            Property : VSS.Strings.Virtual_String;
            Schema   : Schema_Access;
            Optional : Boolean)
         is
            Type_Name : VSS.Strings.Virtual_String := Ref_To_Type_Name (Name);
         begin
            if not Property.Is_Empty then
               Type_Name.Append ("_");
               Type_Name.Append (Property);
            end if;

            Type_Name.Append ("_Variant");

            Write_Union_Variant_Type (Map, Type_Name, Schema);
         end On_Union_Type;
      begin
         Each_Union_Type
           (Map, Optional_Types, On_Union_Type'Access);
      end;

      if not Enum_Package.Is_Empty then
         Put ("end ");
         Put (Enum_Package);
         Put (";");
         New_Line;
         New_Line;
      end if;

      Write_Named_Types
        (Map, Root_Package, Enum_Package, Holders, Optional_Types, Done);

      Write_Vector_Operations (Array_Types, Specification);
      Write_Holders_Operations (Map, Holders, Specification);
      Put ("private");
      New_Line;
      Write_Private_Vectors (Array_Types, Enum_Package);
      Write_Privete_Holders (Map, Holders);
      Put ("end ");
      Put (Root_Package);
      Put (";");
      New_Line;

      Print_Vector (Header);
      Put ("with Ada.Unchecked_Deallocation;");
      New_Line;
      New_Line;
      Put ("package body ");
      Put (Root_Package);
      Put (" is");
      New_Line;
      Write_Vector_Operations (Array_Types, Implemenetation);
      Write_Holders_Operations (Map, Holders, Implemenetation);
      Put ("end ");
      Put (Root_Package);
      Put (";");
      New_Line;
      New_Line;
   end Write_Type_Package;

   ----------------------
   -- Write_Union_Type --
   ----------------------

   procedure Write_Union_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set)
   is
      use type VSS.Strings.Virtual_String;

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property);
      --  Generate anonymous type for given property

      -------------------------
      -- On_Anonymous_Schema --
      -------------------------

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type
           (Name,
            Property,
            Map,
            Optional_Types,
            Done,
            Property.Schema.Required.Contains (Property.Name),
            Root_Package, Enum_Package, Holders);
      end On_Anonymous_Schema;

      Enum_Prefix : constant VSS.Strings.Virtual_String :=
        (if Enum_Package.Is_Empty then VSS.Strings.Empty_Virtual_String
         else Enum_Package & ".");

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name);
   begin
      --  Write dependencies
      for Item of Schema.Any_Of loop
         if not Item.Ref.Is_Empty then
            Write_Named_Type
              (Item.Ref,
               Map (Item.Ref),
               Map,
               Root_Package,
               Enum_Package,
               Holders,
               Optional_Types,
               Done);
         end if;
      end loop;

      Each_Anonymous_Schema (Map, Schema, On_Anonymous_Schema'Access);

      Put ("type ");
      Put (Type_Name);
      Put ("_Union (Kind : ");
      Put (Enum_Prefix);
      Put (Type_Name);
      Put ("_Variant := ");
      Put (Enum_Prefix);
      Put (Type_Name);
      Put ("_Variant'First) is ");
      Put ("record");
      New_Line;
      Put ("case Kind is");
      New_Line;
      for Index in 1 .. Schema.Any_Of.Last_Index loop
         declare
            Variant : constant VSS.Strings.Virtual_String :=
              Escape_Keywords
                (Variant_Name (Map, Schema.Any_Of.Element (Index)));

            Property : constant JSON_Schema.Property :=
             (Variant, Schema.Any_Of.Element (Index));
         begin
            Put ("when ");
            Put (Enum_Prefix);
            Put (Variant);
            Put (" =>");
            New_Line;
            Write_Record_Component
              (Name, Map, Root_Package, Property, True, False);
         end;
      end loop;
      Put ("end case;");
      New_Line;
      Put ("end record;");
      New_Line;
      New_Line;
      --  We need a wrapper around discriminanted type to be able to change
      --  discriminant in Input_Type procedure, otherwise we will get
      --  Constraint_Error exception.
      Put ("type ");
      Put (Type_Name);
      Put (" is record"); New_Line;
      Put ("Union : ");
      Put (Type_Name);
      Put ("_Union;"); New_Line;
      Put ("end record;");
      New_Line;
      New_Line;
   end Write_Union_Type;

   ------------------------------
   -- Write_Union_Variant_Type --
   ------------------------------

   procedure Write_Union_Variant_Type
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access) is
   begin
      Put ("type ");
      Put (Name);

      Put (" is (");

      for Index in 1 .. Schema.Any_Of.Last_Index loop
         if Index > 1 then
            Put (", ");
         end if;

         Put
          (Escape_Keywords
            (Variant_Name (Map, Schema.Any_Of.Element (Index))));
      end loop;

      Put (");");
      New_Line;
      New_Line;
   end Write_Union_Variant_Type;

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
              (if Schema /= null and then Is_Enum (Schema)
               then "Enum." & Item else Item);
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
            Put ("(Self : aliased ");
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

end JSON_Schema.Writers.Types;
