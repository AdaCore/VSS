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
      Done           : in out String_Sets.Set;
      Only_If        : access
        function (Schema : Schema_Access) return Boolean);
   --  Generate types for all named schemas in Map if they match Only_If filter
   --  function and not present in Done already. Include names of generated
   --  type in Done set.

   procedure Write_Named_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  Generate a type for single named Schema. Generate Optional_ type if
   --  corresponding type is included in Optional_Types set. Update Done as
   --  described above.

   procedure Write_Record_Type
     (Name           : VSS.Strings.Virtual_String;
      Schema         : Schema_Access;
      Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set);
   --  The same for "type: object" schema

   procedure Write_Enumeration_Type
     (Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access);
   --  The same for enumeration schema

   procedure Write_Public_Vectors (Array_Types : String_Sets.Set);
   --  Write vector type public declarations for each item of Array_Types

   procedure Write_Private_Vectors (Array_Types : String_Sets.Set);
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
     (Schema : Schema_Access;
      Result : in out String_Sets.Set);
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

   function Is_Enum (Schema : Schema_Access) return Boolean is
     (not Schema.Enum.Is_Empty);
   --  Check for enumeration schema

   function Is_Not_Enum (Schema : Schema_Access) return Boolean is
     (Schema.Enum.Is_Empty);
   --  Check if schema isn't an enumeration

   Reserved_Words : constant VSS.String_Vectors.Virtual_String_Vector :=
     ["function", "interface", "all", "type"];

   Integer_Or_String : constant JSON_Schema.Simple_Type_Vectors.Vector :=
     [Definitions.An_Integer, Definitions.A_String];

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
         return Name;
      elsif Name.Starts_With ("i") or Name.Starts_With ("a") then
         return "an_" & Name;
      else
         return "a_" & Name;
      end if;
   end Escape_Keywords;

   ----------------------
   -- Find_Array_Types --
   ----------------------

   procedure Find_Array_Types
     (Schema : Schema_Access;
      Result : in out String_Sets.Set)
   is
      use type Definitions.Simple_Types;
   begin
      if Schema.Kind.Last_Index = 1
        and then Schema.Kind (1) = Definitions.An_Array
        and then not Schema.Items.First_Element.Ref.Is_Empty
      then
         pragma Assert (Schema.Items.Last_Index = 1);
         Result.Include (Ref_To_Type_Name (Schema.Items.First_Element.Ref));
      end if;

      for Item of Schema.All_Of loop
         if Item.Ref.Is_Empty then
            Find_Array_Types (Item, Result);
         end if;
      end loop;

      for Property of Schema.Properties loop
         Find_Array_Types (Property.Schema, Result);
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
      Optional_Types : String_Sets.Set;
      Array_Types    : String_Sets.Set;
      Done           : String_Sets.Set;
   begin
      for Schema of Map loop
         Find_Optional_Types (Schema, Optional_Types);
         Find_Array_Types (Schema, Array_Types);
      end loop;

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
      Put ("type Integer_Or_String is null record;");
      New_Line;
      Write_Optional_Type ("Integer_Or_String");
      New_Line;
      Put ("type Integer_Vector is tagged private;");
      New_Line;

      Write_Public_Vectors (Array_Types);
      New_Line;
      Put ("package Enum is");
      New_Line;
      New_Line;
      Write_Named_Types (Map, Optional_Types, Done, Only_If => Is_Enum'Access);
      Put ("end Enum;");
      New_Line;
      New_Line;
      Write_Named_Types
        (Map, Optional_Types, Done, Only_If => Is_Not_Enum'Access);
      Put ("private");
      New_Line;
      Put ("type Integer_Vector is tagged null record;");
      New_Line;
      Write_Private_Vectors (Array_Types);
      Put ("end ");
      Put (Package_Name);
      Put (";");
   end Write;

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

   ----------------------------
   -- Write_Enumeration_Type --
   ----------------------------

   procedure Write_Enumeration_Type
     (Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access)
   is
   begin
      Put ("type ");
      Put (Ref_To_Type_Name (Name));
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
         Write_Enumeration_Type (Name, Schema);
      elsif not Schema.Properties.Is_Empty then
         Done.Insert (Name);
         Write_Record_Type (Name, Schema, Map, Optional_Types, Done);
      elsif Schema.Kind.Last_Index = 1
        and then Schema.Kind.First_Element = Definitions.An_Object
      then
         Done.Insert (Name);
         Write_Any_Object (Name);
      end if;

      if Optional_Types.Contains (Ref_To_Type_Name (Name)) then
         Write_Optional_Type (Name);
      end if;
   end Write_Named_Type;

   -----------------------------
   -- Write_Enumeration_Types --
   -----------------------------

   procedure Write_Named_Types
     (Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set;
      Done           : in out String_Sets.Set;
      Only_If           : access
        function (Schema : Schema_Access) return Boolean) is
   begin
      for Cursor in Map.Iterate loop
         declare
            Name : constant VSS.Strings.Virtual_String :=
              JSON_Schema.Readers.Schema_Maps.Key (Cursor);
            Schema : constant Schema_Access :=
              JSON_Schema.Readers.Schema_Maps.Element (Cursor);
         begin
            if Only_If (Schema) then
               Write_Named_Type (Name, Schema, Map, Optional_Types, Done);
            end if;
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

   procedure Write_Private_Vectors (Array_Types : String_Sets.Set) is
   begin
      for Item of Array_Types loop
         Put ("type ");
         Put (Item);
         Put ("_Vector is tagged null record;");
         New_Line;
         New_Line;
      end loop;
   end Write_Private_Vectors;

   --------------------------
   -- Write_Public_Vectors --
   --------------------------

   procedure Write_Public_Vectors (Array_Types : String_Sets.Set) is
   begin
      for Item of Array_Types loop
         Put ("type ");
         Put (Item);
         Put ("_Vector is tagged private;");
         New_Line;
         New_Line;
      end loop;
   end Write_Public_Vectors;

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

      function Field_Type
        (Schema   : Schema_Access;
         Required : Boolean)
         return VSS.Strings.Virtual_String;
      --  Return an Ada type name for given Schema

      function Field_Type
        (Schema   : Schema_Access;
         Required : Boolean)
         return VSS.Strings.Virtual_String
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
                  --  Instead of Optional_String use just Virtual_String,
                  --  to check for an absent value use Is_Null.
                  Result := "VSS.Strings.Virtual_String";

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

               when others =>
                  Result.Append ("XXX");
            end case;
         else
            Result.Append ("YYY");
         end if;

         return Result;
      end Field_Type;
   begin
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
      Put (Ref_To_Type_Name (Name));
      Put (" is record");
      New_Line;

      for Property of Schema.Properties loop
         declare
            Field_Name : constant VSS.Strings.Virtual_String :=
              Escape_Keywords (Property.Name);

            Field_Type : VSS.Strings.Virtual_String :=
              Write_Record_Type.Field_Type
                (Property.Schema, Schema.Required.Contains (Property.Name));
         begin
            if Field_Name.To_Lowercase = Field_Type.To_Lowercase then
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
      end loop;

      Put ("end record;");
      New_Line;
      New_Line;
   end Write_Record_Type;

end JSON_Schema.Writers;
