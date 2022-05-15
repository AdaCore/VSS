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

pragma Warnings (Off, "unrecognized pragma");
pragma Ada_2020;
pragma Extensions_Allowed (On);
pragma Ada_2022;
pragma Warnings (On, "unrecognized pragma");

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Wide_Wide_Text_IO;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

package body JSON_Schema.Writers is

   Reserved_Words : constant VSS.String_Vectors.Virtual_String_Vector :=
   ["function", "interface", "all", "type", "body",
     "private", "protected", "new", "exception", "entry", "goto", "end",
     "boolean"];

   Integer_Or_String : constant JSON_Schema.Simple_Type_Vectors.Vector :=
     [Definitions.An_Integer, Definitions.A_String];

   String_Or_Null : constant JSON_Schema.Simple_Type_Vectors.Vector :=
     [Definitions.A_String, Definitions.A_Null];

   ---------------------------
   -- Each_Anonymous_Schema --
   ---------------------------

   procedure Each_Anonymous_Schema
     (Schema : Schema_Access;
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
   end Each_Anonymous_Schema;

   ---------------------------
   -- Each_Enumeration_Type --
   ---------------------------

   procedure Each_Enumeration_Type
     (Map      : JSON_Schema.Readers.Schema_Map;
      Optional : String_Sets.Set;
      Action   : access procedure
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean))
   is

      procedure Traverse_Nested_Schemas
        (Name     : VSS.Strings.Virtual_String;
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
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean) is
      begin
         if not Schema.Enum.Is_Empty then
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
            Name : constant VSS.Strings.Virtual_String :=
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

   -------------------
   -- Each_Property --
   -------------------

   procedure Each_Property
     (Map    : JSON_Schema.Readers.Schema_Map;
      Schema : Schema_Access;
      Action : access procedure
        (Property : JSON_Schema.Property;
         Required : Boolean))
   is
      use type VSS.Strings.Virtual_String;

      function Equal_Name (Left, Right : JSON_Schema.Property) return Boolean
        is (Left.Name = Right.Name);

      package Property_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Property, Equal_Name);

      procedure Prepend
        (List     : in out Property_Lists.List;
         Property : JSON_Schema.Property);
      --  If property in the list, then move it at the beginning, otherwise
      --  prepend it to the list.

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (List     : in out Property_Lists.List;
         Property : JSON_Schema.Property)
      is
         Cursor : Property_Lists.Cursor := List.Find (Property);
      begin
         if Property_Lists.Has_Element (Cursor) then
            declare
               Value : constant JSON_Schema.Property :=
                 Property_Lists.Element (Cursor);
            begin
               List.Delete (Cursor);
               List.Prepend (Value);
            end;
         else
            List.Prepend (Property);
         end if;
      end Prepend;

      List : Property_Lists.List;
      --  A list of collected properties. Items in the list are ordered.
      --  If we found a property in a parent schema then we move the property
      --  to the beginning of the list instead of creating a new one.

      Required : String_Sets.Set;
      --  Set of required properties

      Next : Schema_Access := Schema;
   begin
      loop
         for Property of reverse Next.Properties loop
            Prepend (List, Property);

            if Next.Required.Contains (Property.Name) then
               Required.Include (Property.Name);
            end if;
         end loop;

         for Item of Next.All_Of loop
            for Property of reverse Item.Properties loop
               Prepend (List, Property);

               if Item.Required.Contains (Property.Name) then
                  Required.Include (Property.Name);
               end if;
            end loop;
         end loop;

         if Next.All_Of.Is_Empty then
            exit;
         else
            Next := Map (Next.All_Of.First_Element.Ref);
         end if;
      end loop;

      for Property of List loop
         Action (Property, Required.Contains (Property.Name));
      end loop;
   end Each_Property;

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
              Name.At_First_Character;
         begin
            if Cursor.Forward and then Cursor.Forward then
               return Name.Slice (Cursor, Name.At_Last_Character);
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

         if not Map (Schema.Ref).Enum.Is_Empty then
            Prefix := "Enum.";
         end if;

      elsif not Schema.Additional_Properties.Is_Boolean
        and then Schema.Additional_Properties.Schema /= null
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

               if Required and Schema.Enum.Length = 1 then
                  --  If string type redefined as an enum with just one literal
                  --  then skip this property by returning an empty type name.
                  Result := VSS.Strings.Empty_Virtual_String;
               elsif Schema.Enum.Length > 1 then
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

end JSON_Schema.Writers;
