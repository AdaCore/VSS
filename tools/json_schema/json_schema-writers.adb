--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Wide_Wide_Text_IO;

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

package body JSON_Schema.Writers is

   Reserved_Words : constant VSS.String_Vectors.Virtual_String_Vector :=
     ["abort", "abs", "abstract", "accept", "access", "aliased", "all", "and",
      "array", "at", "begin", "body", "case", "constant", "declare", "delay",
      "delta", "digits", "do", "else", "elsif", "end", "entry", "exception",
      "exit", "for", "function", "generic", "goto", "if", "in", "interface",
      "is", "limited", "loop", "mod", "new", "not", "null", "of", "or",
      "others", "out", "overriding", "package", "pragma", "private",
      "procedure", "protected", "raise", "range", "record", "rem", "renames",
      "requeue", "return", "reverse", "select", "separate", "some", "subtype",
      "synchronized", "tagged", "task", "terminate", "then", "type", "until",
      "use", "when", "while", "with", "xor",
      "boolean"];

   Integer_Or_String : constant JSON_Schema.Simple_Type_Vectors.Vector :=
     [Definitions.An_Integer, Definitions.A_String];

   String_Or_Null : constant JSON_Schema.Simple_Type_Vectors.Vector :=
     [Definitions.A_String, Definitions.A_Null];

   ---------------------------
   -- Each_Anonymous_Schema --
   ---------------------------

   procedure Each_Anonymous_Schema
     (Map    : JSON_Schema.Readers.Schema_Map;
      Schema : Schema_Access;
      Action : access procedure (Property : JSON_Schema.Property)) is
   begin
      for Used of Schema.All_Of loop
         for Property of Used.Properties loop
            if Property.Schema.Kind.Last_Index = 1 then
               case Property.Schema.Kind (1) is
                  when Definitions.An_Object =>
                     Action (Property);
                  when others =>
                     null;
               end case;
            end if;
         end loop;
      end loop;

      for Used of Schema.Any_Of loop
         if Used.Kind.Last_Index = 1 then
            case Used.Kind (1) is
               when Definitions.An_Object =>
                  Action ((Escape_Keywords (Variant_Name (Map, Used)), Used));
               when others =>
                  null;
            end case;
         end if;
      end loop;
   end Each_Anonymous_Schema;

   ---------------------------
   -- Each_Enumeration_Type --
   ---------------------------

   procedure Each_Enumeration_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Optional : String_Sets.Set;
      Action   : access procedure
        (Name     : Schema_Name;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean))
   is

      procedure Traverse_Nested_Schemas
        (Name     : Schema_Name;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean);
      --  Traverse all types in Schema recursively, find enumeration schemes
      --  and call Action for them. Name is toppest named schema, while
      --  Property (if not empty) is a property corresponding to Schema.

      -----------------------------
      -- Traverse_Nested_Schemas --
      -----------------------------

      procedure Traverse_Nested_Schemas
        (Name     : Schema_Name;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean) is
      begin
         if Is_Enum (Schema) then
            Action (Name, Property, Schema, Optional);
         end if;

         for Item of Schema.All_Of loop
            if Item.Ref.Is_Empty then
               Traverse_Nested_Schemas (Name, Property, Item, True);
            end if;
         end loop;

         for Item of Schema.Any_Of loop
            if Item.Ref.Is_Empty then
               Traverse_Nested_Schemas (Name, Property, Item, True);
            end if;
         end loop;

         for Property of Schema.Properties loop
            Traverse_Nested_Schemas
              (Name,
               Property.Name,
               Property.Schema,
               not Schema.Required.Contains (Property.Name));
         end loop;
      end Traverse_Nested_Schemas;

   begin
      for Cursor in Map.Iterate loop
         declare
            Name : constant Schema_Name :=
              JSON_Schema.Readers.Schema_Maps.Key (Cursor);

            Schema : constant Schema_Access :=
              JSON_Schema.Readers.Schema_Maps.Element (Cursor);

            Type_Name : constant VSS.Strings.Virtual_String :=
              Ref_To_Type_Name (Name);
         begin
            Traverse_Nested_Schemas
              (Name,
               "",
               Schema,
               Optional.Contains (Type_Name));
         end;
      end loop;
   end Each_Enumeration_Type;

   ----------------------
   -- Each_Holder_Type --
   ----------------------

   procedure Each_Holder_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Holders  : VSS.String_Vectors.Virtual_String_Vector;
      Action   : access procedure (Name : Schema_Name))
   is
      Done : String_Sets.Set;
   begin
      for Item of Holders loop
         declare
            use type VSS.Strings.Virtual_String;

            Pair   : constant VSS.String_Vectors.Virtual_String_Vector :=
              Item.Split (Separator => ':');

            Schema : constant Schema_Access := Map (Pair (1));
         begin
            for Property of Schema.Properties loop
               if Property.Name = Pair (2) then
                  declare
                     Name : constant Schema_Name := Property.Schema.Ref;
                  begin
                     if not Done.Contains (Name) then
                        Action (Name);
                        Done.Include (Name);
                     end if;
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Each_Holder_Type;

   -------------------
   -- Each_Property --
   -------------------

   procedure Each_Property
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : Schema_Name;
      Schema : Schema_Access;
      Action : access procedure
        (Enclosing : Schema_Name;
         Property  : JSON_Schema.Property;
         Required  : Boolean))
   is
      use type VSS.Strings.Virtual_String;

      type Property_Info is record
         Schema   : Schema_Name;
         Property : JSON_Schema.Property;
      end record;

      function Equal_Name (Left, Right : Property_Info)
        return Boolean is (Left.Property.Name = Right.Property.Name);

      package Property_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Property_Info, Equal_Name);

      procedure Prepend
        (List     : in out Property_Lists.List;
         Schema   : Schema_Name;
         Property : JSON_Schema.Property);
      --  If property in the list, then move it at the beginning, otherwise
      --  prepend it to the list.

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (List     : in out Property_Lists.List;
         Schema   : Schema_Name;
         Property : JSON_Schema.Property)
      is
         Cursor : Property_Lists.Cursor := List.Find (("", Property));
      begin
         if Property_Lists.Has_Element (Cursor) then
            declare
               Value : constant Property_Info :=
                 Property_Lists.Element (Cursor);
            begin
               List.Delete (Cursor);
               List.Prepend (Value);
            end;
         else
            List.Prepend ((Schema, Property));
         end if;
      end Prepend;

      List : Property_Lists.List;
      --  A list of collected properties. Items in the list are ordered.
      --  If we found a property in a parent schema then we move the property
      --  to the beginning of the list instead of creating a new one.

      Required : String_Sets.Set;
      --  Set of required properties

      Next : Schema_Access := Schema;

      Enclosing : Schema_Name := Name;  --  Name of Next
   begin
      loop
         for Property of reverse Next.Properties loop
            Prepend (List, Enclosing, Property);

            if Next.Required.Contains (Property.Name) then
               Required.Include (Property.Name);
            end if;
         end loop;

         for Item of Next.All_Of loop
            for Property of reverse Item.Properties loop
               Prepend (List, Enclosing, Property);

               if Item.Required.Contains (Property.Name) then
                  Required.Include (Property.Name);
               end if;
            end loop;
         end loop;

         if Next.All_Of.Is_Empty then
            exit;
         else
            Enclosing := Next.All_Of.First_Element.Ref;
            Next := Map (Enclosing);
         end if;
      end loop;

      for Item of List loop
         Action
           (Item.Schema,
            Item.Property,
            Required.Contains (Item.Property.Name));
      end loop;
   end Each_Property;

   ---------------------
   -- Each_Union_Type --
   ---------------------

   procedure Each_Union_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Optional : String_Sets.Set;
      Action   : access procedure
        (Name     : Schema_Name;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean))
   is

      procedure Traverse_Nested_Schemas
        (Name     : Schema_Name;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean);
      --  Traverse all types in Schema recursively, find anyOf schemes
      --  and call Action for them. Name is toppest named schema, while
      --  Property (if not empty) is a property corresponding to Schema.

      -----------------------------
      -- Traverse_Nested_Schemas --
      -----------------------------

      procedure Traverse_Nested_Schemas
        (Name     : Schema_Name;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean)
      is
      begin
         if Is_Union_Type (Schema) then
            Action (Name, Property, Schema, Optional);
         end if;

         for Item of Schema.All_Of loop
            if Item.Ref.Is_Empty then
               Traverse_Nested_Schemas (Name, Property, Item, True);
            end if;
         end loop;

         for Property of Schema.Properties loop
            Traverse_Nested_Schemas
              (Name,
               Property.Name,
               Property.Schema,
               not Schema.Required.Contains (Property.Name));
         end loop;
      end Traverse_Nested_Schemas;

   begin
      for Cursor in Map.Iterate loop
         declare
            Name : constant Schema_Name :=
              JSON_Schema.Readers.Schema_Maps.Key (Cursor);

            Schema : constant Schema_Access :=
              JSON_Schema.Readers.Schema_Maps.Element (Cursor);

            Type_Name : constant VSS.Strings.Virtual_String :=
              Ref_To_Type_Name (Name);
         begin
            Traverse_Nested_Schemas
              (Name,
               "",
               Schema,
               Optional.Contains (Type_Name));
         end;
      end loop;
   end Each_Union_Type;

   ---------------------
   -- Escape_Keywords --
   ---------------------

   function Escape_Keywords (Name : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String
   is
      use type VSS.Strings.Virtual_String;
   begin
      if Name.At_First_Character.Element not in 'A' .. 'Z' | 'a' .. 'z' then
         --  Trim any non-alpha character at the begining of Name
         declare
            Char   : VSS.Characters.Virtual_Character'Base;
            Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
              Name.At_First_Character;
         begin
            while Cursor.Forward (Char) loop
               exit when Char in 'A' .. 'Z' | 'a' .. 'z';
            end loop;

            return Name.Slice (Cursor, Name.At_Last_Character);
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

   ----------------------
   -- Get_Element_Type --
   ----------------------

   procedure Get_Element_Type
     (Map       : JSON_Schema.Readers.Schema_Map;
      Schema    : Schema_Access;
      Type_Name : out VSS.Strings.Virtual_String;
      Prefix    : out VSS.Strings.Virtual_String) is
   begin
      if Schema.Kind.Last_Index = 1 then
         case Schema.Kind (1) is
            when Definitions.An_Array =>
               Get_Field_Type
                 (Map, Schema.Items.First_Element,
                  True, "", Type_Name, Prefix);

               if Type_Name.Is_Empty then
                  Type_Name := "Virtual_String";
                  Prefix := "VSS.Strings.";
               end if;
            when others =>
               null;
         end case;
      end if;
   end Get_Element_Type;

   --------------------
   -- Get_Field_Type --
   --------------------

   procedure Get_Field_Type
     (Map       : JSON_Schema.Readers.Schema_Map;
      Schema    : Schema_Access;
      Required  : Boolean;
      Fallback  : VSS.Strings.Virtual_String;
      Type_Name : out VSS.Strings.Virtual_String;
      Prefix    : out VSS.Strings.Virtual_String)
   is
      use type JSON_Schema.Simple_Type_Vectors.Vector;

      Result : VSS.Strings.Virtual_String :=
        (if Required then VSS.Strings.Empty_Virtual_String
         else "Optional_");
   begin
      if not Schema.Ref.Is_Empty then
         Result.Append (Ref_To_Type_Name (Schema.Ref));

         if Is_Enum (Map (Schema.Ref)) then
            Prefix := "Enum.";
         end if;

      elsif Schema.Additional_Properties /= null and then not
         Schema.Additional_Properties.Is_False
      then
         Result := "Any_Object";  --  TODO: Make more precise type???
      elsif Schema.Kind.Last_Index = 7 then
         Result := "Any_Value";
      elsif Schema.Kind = Integer_Or_String then
         Result.Append ("Integer_Or_String");
      elsif Schema.Kind = String_Or_Null then
         --  To represent `null` it uses Is_Null function
         Result.Append ("Virtual_String");
         Prefix := "VSS.Strings.";
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

               if Required and
                 (Schema.Enum.Length = 1
                  or Schema.X_Enum.Length = 1
                  or not Schema.Const.Is_Empty)
               then
                  --  If string type redefined as an enum with just one literal
                  --  then skip this property by returning an empty type name.
                  Result := VSS.Strings.Empty_Virtual_String;
               elsif Is_Enum (Schema) then
                  Prefix := "Enum.";
                  Result.Append (Fallback);
               else
                  --  Instead of Optional_String use just Virtual_String,
                  --  to check for an absent value use Is_Null.
                  Result := "Virtual_String";
                  Prefix := "VSS.Strings.";
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
                           Result := "Virtual_String_Vector";
                           Prefix := "VSS.String_Vectors.";
                     end case;
                  end if;
               end;

            when Definitions.An_Object =>
               Result.Append (Fallback);
         end case;
      else
         Result.Append ("YYY");
      end if;

      Type_Name := Result;
   end Get_Field_Type;

   ---------------------
   -- Is_Holder_Field --
   ---------------------

   function Is_Holder_Field
     (Name     : Schema_Name;
      Property : VSS.Strings.Virtual_String;
      Holders  : VSS.String_Vectors.Virtual_String_Vector) return Boolean
   is
      use type VSS.Strings.Virtual_String;
   begin
      return Holders.Contains (Name & ":" & Property);
   end Is_Holder_Field;

   -------------------
   -- Is_Union_Type --
   -------------------

   function Is_Union_Type (Schema : Schema_Access) return Boolean is
     (not (for all S of Schema.Any_Of => S.Is_True and S.Ref.Is_Empty));

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Ada.Wide_Wide_Text_IO.New_Line;
   end New_Line;

   ------------------
   -- Print_Vector --
   ------------------

   procedure Print_Vector
     (Header : VSS.String_Vectors.Virtual_String_Vector) is
   begin
      for Item of Header loop
         Put (Item);
         New_Line;
      end loop;
   end Print_Vector;

   ---------
   -- Put --
   ---------

   procedure Put (Text : VSS.Strings.Virtual_String) is
   begin
      Ada.Wide_Wide_Text_IO.Put
        (VSS.Strings.Conversions.To_Wide_Wide_String (Text));
   end Put;

   ----------------------
   -- Ref_To_Type_Name --
   ----------------------

   function Ref_To_Type_Name (Subschema : Schema_Name)
     return VSS.Strings.Virtual_String
   is
      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        Subschema.Split ('/');
   begin
      return List.Element (List.Length);
   end Ref_To_Type_Name;

   ------------------
   -- Variant_Name --
   ------------------

   function Variant_Name
     (Map       : JSON_Schema.Readers.Schema_Map;
      Schema    : Schema_Access) return VSS.Strings.Virtual_String
   is
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

   begin
      if Schema.Ref.Is_Empty then
         for Property of Schema.Properties loop
            --  Look for the first string `const` property
            if not Property.Schema.Const.Is_Empty and then
              Property.Schema.Const.First_Element.Kind = String_Value
            then

               return Property.Schema.Const.First_Element.String_Value;
            end if;
         end loop;

         raise Program_Error with "No const found in anyOf item.";
      else
         return Variant_Name (Map, Map (Schema.Ref));
      end if;
   end Variant_Name;

end JSON_Schema.Writers;
