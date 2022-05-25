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

package body JSON_Schema.Writers.Outputs is

   procedure Write_Named_Type
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access;
      Kind   : Declaration_Kind);

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map);

   procedure Write_Output_Specification
     (Type_Name : VSS.Strings.Virtual_String;
      Prefix    : VSS.Strings.Virtual_String);

   procedure Write_Record_Component
     (Name     : VSS.Strings.Virtual_String;
      Map      : JSON_Schema.Readers.Schema_Map;
      Property : JSON_Schema.Property;
      Required : Boolean);

   ----------------------
   -- Generate_Writers --
   ----------------------

   procedure Generate_Writers
     (Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Header         : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set)
   is
      use type VSS.Strings.Virtual_String;

      procedure Write_Enum_Specification
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean);
      --  Generate Output procedure specification for an enumeration schema

      procedure Write_Enum_Body
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean);
      --  Generate Output procedure body for an enumeration schema

      Enum_Prefix : constant VSS.Strings.Virtual_String :=
        (if Enum_Package.Is_Empty then VSS.Strings.Empty_Virtual_String
         else Enum_Package & ".");

      ---------------------
      -- Write_Enum_Body --
      ---------------------

      procedure Write_Enum_Body
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean)
      is
         pragma Unreferenced (Optional);

         Type_Name : VSS.Strings.Virtual_String := Ref_To_Type_Name (Name);
      begin
         if Schema.Enum.Length > 1 then
            if not Property.Is_Empty then
               Type_Name.Append ("_");
               Type_Name.Append (Property);
            end if;

            Write_Output_Specification (Type_Name, "Enum.");
            Put (" is");
            New_Line;
            Put ("begin");
            New_Line;
            Put ("case Value is");
            New_Line;
            for Index in 1 .. Schema.Enum.Length loop
               Put ("when ");
               Put (Enum_Prefix);
               Put (Escape_Keywords (Schema.Enum.Element (Index)));
               Put (" =>");
               New_Line;
               Put ("Handler.String_Value (""");
               Put (Schema.Enum.Element (Index));
               Put (""");");
               New_Line;
            end loop;
            Put ("end case;");
            New_Line;

            Put ("end Output_");
            Put (Type_Name);
            Put (";");
            New_Line;
            New_Line;
         end if;
      end Write_Enum_Body;

      -------------------------
      -- On_Enumeration_Type --
      -------------------------

      procedure Write_Enum_Specification
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean)
      is
         pragma Unreferenced (Optional);

         Type_Name : VSS.Strings.Virtual_String := Ref_To_Type_Name (Name);
      begin
         if Schema.Enum.Length > 1 then
            if not Property.Is_Empty then
               Type_Name.Append ("_");
               Type_Name.Append (Property);
            end if;

            Write_Output_Specification (Type_Name, Enum_Prefix);
            Put (";");
            New_Line;
            New_Line;
         end if;
      end Write_Enum_Specification;

   begin
      Print_Vector (Header);
      Put ("with VSS.JSON.Content_Handlers;");
      New_Line;
      New_Line;
      Put ("package ");
      Put (Root_Package);
      Put (".Outputs is");
      New_Line;
      New_Line;

      Each_Enumeration_Type
        (Map, Optional_Types, Write_Enum_Specification'Access);

      for Cursor in Map.Iterate loop
         Write_Named_Type
           (Map,
            JSON_Schema.Readers.Schema_Maps.Key (Cursor),
            JSON_Schema.Readers.Schema_Maps.Element (Cursor),
            Specification);
      end loop;

      Put ("end ");
      Put (Root_Package);
      Put (".Outputs;");
      New_Line;

      Print_Vector (Header);
      Put ("with Interfaces;"); New_Line;
      Put ("package body ");
      Put (Root_Package);
      Put (".Outputs is");
      New_Line;
      Put ("pragma Style_Checks (Off);");
      New_Line;

      Put ("procedure Output_Any_Value"); New_Line;
      Put ("  (Handler : in out ");
      Put ("VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;"); New_Line;
      Put ("   Value   : Any_Value'Class) is"); New_Line;
      Put ("begin"); New_Line;
      Put ("   for Item of Value loop"); New_Line;
      Put ("      case Item.Kind is"); New_Line;
      Put ("         when VSS.JSON.Events.Start_Array =>"); New_Line;
      Put ("            Handler.Start_Array;"); New_Line;
      Put ("         when VSS.JSON.Events.End_Array =>"); New_Line;
      Put ("            Handler.End_Array;"); New_Line;
      Put ("         when VSS.JSON.Events.Start_Object =>"); New_Line;
      Put ("            Handler.Start_Object;"); New_Line;
      Put ("         when VSS.JSON.Events.End_Object =>"); New_Line;
      Put ("            Handler.End_Object;"); New_Line;
      Put ("         when VSS.JSON.Events.Key_Name =>"); New_Line;
      Put ("            Handler.Key_Name (Item.Key);"); New_Line;
      Put ("         when VSS.JSON.Events.String_Value =>"); New_Line;
      Put ("            Handler.String_Value (Item.String_Value);"); New_Line;
      Put ("         when VSS.JSON.Events.Number_Value =>"); New_Line;
      Put ("            Handler.Number_Value (Item.Number_Value);"); New_Line;
      Put ("         when VSS.JSON.Events.Boolean_Value =>"); New_Line;
      Put ("            Handler.Boolean_Value (Item.Boolean_Value);");
      New_Line;
      Put ("         when VSS.JSON.Events.Null_Value =>"); New_Line;
      Put ("            Handler.Null_Value;"); New_Line;
      Put ("         when VSS.JSON.Events.None =>"); New_Line;
      Put ("            null;"); New_Line;
      Put ("      end case;"); New_Line;
      Put ("   end loop;"); New_Line;
      Put ("end Output_Any_Value;");
      New_Line;
      New_Line;

      Each_Enumeration_Type (Map, Optional_Types, Write_Enum_Body'Access);

      for Cursor in Map.Iterate loop
         Write_Named_Type
           (Map,
            JSON_Schema.Readers.Schema_Maps.Key (Cursor),
            JSON_Schema.Readers.Schema_Maps.Element (Cursor),
            Implemenetation);
      end loop;

      Put ("end ");
      Put (Root_Package);
      Put (".Outputs;");
      New_Line;
   end Generate_Writers;

   --------------------------
   -- Write_Anonymous_Type --
   --------------------------

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map)
   is
      use type VSS.Strings.Virtual_String;

      procedure On_Property
        (Property : JSON_Schema.Property;
         Required : Boolean);
      --  Generate component output code for given property

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property);
      --  Generate anonymous type for given property

      -------------------------
      -- On_Anonymous_Schema --
      -------------------------

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type (Enclosing_Type, Property, Map);
      end On_Anonymous_Schema;

      -----------------
      -- On_Property --
      -----------------

      procedure On_Property
        (Property : JSON_Schema.Property;
         Required : Boolean) is
      begin
         Write_Record_Component (Enclosing_Type, Map, Property, Required);
      end On_Property;

      Schema : Schema_Access renames Property.Schema;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Enclosing_Type)
        & "_" & Property.Name;
   begin
      Write_Output_Specification (Type_Name, "");
      Put (" is");
      New_Line;

      --  Write output procedures for anonymous schemas
      Each_Anonymous_Schema (Schema, On_Anonymous_Schema'Access);

      Put ("begin");
      New_Line;

      if not Schema.All_Of.Is_Empty then
         Put ("Handler.Start_Object;");
         New_Line;
         Writers.Each_Property (Map, Schema, On_Property'Access);
         Put ("Handler.End_Object;");
         New_Line;
      elsif not Schema.Properties.Is_Empty then
         Put ("Handler.Start_Object;");
         New_Line;
         for Property of Schema.Properties loop
            Write_Record_Component
              (Enclosing_Type,
               Map,
               Property,
               Schema.Required.Contains (Property.Name));
         end loop;
         Put ("Handler.End_Object;");
         New_Line;
      else
         Put ("Output_Any_Value (Handler, Value);");
         New_Line;
      end if;

      Put ("end Output_");
      Put (Type_Name);
      Put (";");
      New_Line;
      New_Line;
   end Write_Anonymous_Type;

   ----------------------
   -- Write_Named_Type --
   ----------------------

   procedure Write_Named_Type
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access;
      Kind   : Declaration_Kind)
   is
      procedure On_Property
        (Property : JSON_Schema.Property;
         Required : Boolean);
      --  Generate component output code for given property

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property);
      --  Generate anonymous type for given property

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name);

      -------------------------
      -- On_Anonymous_Schema --
      -------------------------

      procedure On_Anonymous_Schema (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type (Name, Property, Map);
      end On_Anonymous_Schema;

      -----------------
      -- On_Property --
      -----------------

      procedure On_Property
        (Property : JSON_Schema.Property;
         Required : Boolean) is
      begin
         Write_Record_Component (Name, Map, Property, Required);
      end On_Property;
   begin
      if not Schema.Enum.Is_Empty then
         return;
      end if;

      Write_Output_Specification (Type_Name, "");

      if Kind = Specification then
         Put (";");
         New_Line;
         New_Line;
         return;
      end if;

      Put (" is");
      New_Line;

      --  Write output procedures for anonymous schemas
      Each_Anonymous_Schema (Schema, On_Anonymous_Schema'Access);

      Put ("begin");
      New_Line;

      if not Schema.All_Of.Is_Empty then
         Put ("Handler.Start_Object;");
         New_Line;
         Writers.Each_Property (Map, Schema, On_Property'Access);
         Put ("Handler.End_Object;");
         New_Line;
      elsif not Schema.Properties.Is_Empty then
         Put ("Handler.Start_Object;");
         New_Line;
         for Property of Schema.Properties loop
            Write_Record_Component
              (Name,
               Map,
               Property,
               Schema.Required.Contains (Property.Name));
         end loop;
         Put ("Handler.End_Object;");
         New_Line;
      else
         Put ("Output_Any_Value (Handler, Value);");
         New_Line;
      end if;

      Put ("end Output_");
      Put (Type_Name);
      Put (";");
      New_Line;
      New_Line;
   end Write_Named_Type;

   --------------------------------
   -- Write_Output_Specification --
   --------------------------------

   procedure Write_Output_Specification
     (Type_Name : VSS.Strings.Virtual_String;
      Prefix    : VSS.Strings.Virtual_String) is
   begin
      Put ("procedure Output_");
      Put (Type_Name);
      New_Line;
      Put ("(Handler : in out ");
      Put ("VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;");
      New_Line;
      Put ("Value : ");
      Put (Prefix);
      Put (Type_Name);
      Put (")");
   end Write_Output_Specification;

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
      use all type VSS.JSON.Events.JSON_Event_Kind;

      procedure Write_Key_And_Value
        (Field_Name : VSS.Strings.Virtual_String;
         Type_Name  : VSS.Strings.Virtual_String;
         Suffix     : VSS.Strings.Virtual_String);
      --  Generate Key_Name and Output_XXX calls

      procedure Write_Value
        (Field_Name : VSS.Strings.Virtual_String;
         Type_Name  : VSS.Strings.Virtual_String;
         Suffix     : VSS.Strings.Virtual_String);
      --  Generate Key_Name and Output_XXX calls

      -------------------------
      -- Write_Key_And_Value --
      -------------------------

      procedure Write_Key_And_Value
        (Field_Name : VSS.Strings.Virtual_String;
         Type_Name  : VSS.Strings.Virtual_String;
         Suffix     : VSS.Strings.Virtual_String) is
      begin
         Put ("Handler.Key_Name (""");
         Put (Property.Name);
         Put (""");");
         New_Line;
         Write_Value (Field_Name, Type_Name, Suffix);
      end Write_Key_And_Value;

      -----------------
      -- Write_Value --
      -----------------

      procedure Write_Value
        (Field_Name : VSS.Strings.Virtual_String;
         Type_Name  : VSS.Strings.Virtual_String;
         Suffix     : VSS.Strings.Virtual_String) is
      begin
         if Type_Name = "Any_Object" then
            Write_Value (Field_Name, "Any_Value", Suffix);
         elsif Type_Name = "Virtual_String" then
            Put ("Handler.String_Value (Value.");
            Put (Field_Name);
            Put (Suffix);
            Put (");");
            New_Line;
         elsif Type_Name = "Integer" then
            Put ("Handler.Integer_Value");
            Put ("(Interfaces.Integer_64 (Integer'(Value.");
            Put (Field_Name);
            Put (Suffix);
            Put (")));");
            New_Line;
         elsif Type_Name = "Float" then
            Put ("Handler.Float_Value");
            Put ("(Interfaces.IEEE_Float_64 (Value.");
            Put (Field_Name);
            Put (Suffix);
            Put ("));");
            New_Line;
         elsif Type_Name = "Boolean" then
            Put ("Handler.Boolean_Value (Value.");
            Put (Field_Name);
            Put (Suffix);
            Put (");");
            New_Line;
         elsif Type_Name.Ends_With ("_Vector") then
            declare
               Item_Type   : VSS.Strings.Virtual_String;
               Type_Prefix : VSS.Strings.Virtual_String;
            begin
               Get_Element_Type
                 (Map, Property.Schema, Item_Type, Type_Prefix);

               Put ("Handler.Start_Array;");
               New_Line;
               Put ("for J in 1 .. Value.");
               Put (Field_Name);
               Put (".Length loop");
               New_Line;
               Write_Value (Field_Name, Item_Type, " (J)");
               Put ("end loop;");
               New_Line;
               Put ("Handler.End_Array;");
               New_Line;
            end;
         elsif Type_Name = "Integer_Or_String" then
            Put ("if Value.");
            Put (Field_Name);
            Put (Suffix);
            Put (".Is_String then");
            New_Line;
            Write_Value (Field_Name, "Virtual_String", Suffix & ".String");
            Put ("else");
            New_Line;
            Write_Value (Field_Name, "Integer", Suffix & ".Integer");
            Put ("end if;");
            New_Line;
         else
            Put ("Output_");
            Put (Type_Name);
            Put (" (Handler, Value.");
            Put (Field_Name);
            Put (Suffix);
            Put (");");
            New_Line;
         end if;
      end Write_Value;

      Fallback : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name) & "_" & Property.Name;

      Field_Name : constant VSS.Strings.Virtual_String :=
        Escape_Keywords (Property.Name);

      Type_Name   : VSS.Strings.Virtual_String;
      Type_Prefix : VSS.Strings.Virtual_String;
   begin
      Get_Field_Type
        (Map, Property.Schema, True, Fallback, Type_Name, Type_Prefix);

      if Property.Schema.Enum.Length = 1 then
         --  Write constant property (single item enum)
         Put ("Handler.Key_Name (""");
         Put (Property.Name);
         Put (""");");
         Put ("Handler.String_Value (""");
         Put (Property.Schema.Enum.Element (1));
         Put (""");");
         New_Line;
      elsif not Property.Schema.Const.Is_Empty then
         --  Write constant property

         --  Only string constant is supported for now.
         pragma Assert
          (Property.Schema.Const.First_Element.Kind = String_Value);

         Put ("Handler.Key_Name (""");
         Put (Property.Name);
         Put (""");");
         Put ("Handler.String_Value (""");
         Put (Property.Schema.Const.First_Element.String_Value);
         Put (""");");
         New_Line;
      elsif not Required and Type_Name = "Virtual_String" then
         Put ("if not Value.");
         Put (Field_Name);
         Put (".Is_Null then");
         New_Line;

         Write_Key_And_Value (Field_Name, Type_Name, "");

         Put ("end if;");
         New_Line;
      elsif not Required and Type_Name = "Virtual_String_Vector" then
         Put ("if not Value.");
         Put (Field_Name);
         Put (".Is_Empty then");
         New_Line;

         Write_Key_And_Value (Field_Name, Type_Name, "");

         Put ("end if;");
         New_Line;

      elsif not Required and Type_Name.Ends_With ("_Vector") then
         Put ("if Value.");
         Put (Field_Name);
         Put (".Length > 0 then");
         New_Line;

         Write_Key_And_Value (Field_Name, Type_Name, "");

         Put ("end if;");
         New_Line;
      elsif not Required and Type_Name = "Boolean" then
         Put ("if Value.");
         Put (Field_Name);
         Put (" then");
         New_Line;

         Write_Key_And_Value (Field_Name, Type_Name, "");

         Put ("end if;");
         New_Line;
      elsif not Required and
         (Property.Schema.Kind.Last_Index = 7 or
           Property.Schema.Additional_Properties /= null)
      then
         Put ("if not Value.");
         Put (Field_Name);
         Put (".Is_Empty then");
         New_Line;

         Write_Key_And_Value (Field_Name, Type_Name, "");

         Put ("end if;");
         New_Line;
      elsif Required then
         Write_Key_And_Value (Field_Name, Type_Name, "");
      else
         Put ("if Value.");
         Put (Field_Name);
         Put (".Is_Set then");
         New_Line;
         Write_Key_And_Value (Field_Name, Type_Name, ".Value");
         Put ("end if;");
         New_Line;
      end if;
   end Write_Record_Component;

end JSON_Schema.Writers.Outputs;
