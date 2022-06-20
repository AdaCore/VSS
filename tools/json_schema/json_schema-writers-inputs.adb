--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Conversions;

package body JSON_Schema.Writers.Inputs is

   procedure Write_Hash
     (Map       : JSON_Schema.Readers.Schema_Map;
      Type_Name : VSS.Strings.Virtual_String;
      Schema    : Schema_Access);

   procedure Write_Named_Type
     (Map          : JSON_Schema.Readers.Schema_Map;
      Enum_Package : VSS.Strings.Virtual_String;
      Name         : VSS.Strings.Virtual_String;
      Schema       : Schema_Access;
      Kind         : Declaration_Kind;
      Holders      : VSS.String_Vectors.Virtual_String_Vector);

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map;
      Holders        : VSS.String_Vectors.Virtual_String_Vector);

   procedure Write_Object_Reader
     (Map     : JSON_Schema.Readers.Schema_Map;
      Name    : VSS.Strings.Virtual_String;
      Suffix  : VSS.Strings.Virtual_String;
      Schema  : Schema_Access;
      Holders : VSS.String_Vectors.Virtual_String_Vector);

   procedure Write_Union_Reader
     (Map          : JSON_Schema.Readers.Schema_Map;
      Name         : VSS.Strings.Virtual_String;
      Schema       : Schema_Access;
      Enum_Package : VSS.Strings.Virtual_String);

   procedure Write_Input_Specification
     (Type_Name : VSS.Strings.Virtual_String;
      Prefix    : VSS.Strings.Virtual_String);

   procedure Write_Record_Component
     (Name     : VSS.Strings.Virtual_String;
      Map      : JSON_Schema.Readers.Schema_Map;
      Property : JSON_Schema.Property;
      Required : Boolean;
      Holders  : VSS.String_Vectors.Virtual_String_Vector);

   procedure Put (Value : Integer);

   ----------------------
   -- Generate_Readers --
   ----------------------

   procedure Generate_Readers
     (Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Header         : VSS.String_Vectors.Virtual_String_Vector;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
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

      procedure Write_Variant_Hash
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean);
      --  Generate Hash package for variant values in anyOf Schema

      procedure Count_Union_Types
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean);
      --  Increment number of anyOf types

      Enum_Prefix : constant VSS.Strings.Virtual_String :=
        (if Enum_Package.Is_Empty then VSS.Strings.Empty_Virtual_String
         else Enum_Package & ".");

      Union_Types_Count : Natural := 0;  --  Number of anyOf schemas

      procedure Count_Union_Types
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean) is
      begin
         Union_Types_Count := Union_Types_Count + 1;
      end Count_Union_Types;

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

            Put ("package ");
            Put (Type_Name);
            Put ("_Minimal_Perfect_Hash is new Minimal_Perfect_Hash ([");

            for Index in 1 .. Schema.Enum.Length loop
               if Index > 1 then
                  Put (", ");
               end if;
               Put ("""");
               Put (Schema.Enum.Element (Index));
               Put ("""");
            end loop;

            Put ("]);");
            New_Line;
            New_Line;

            Write_Input_Specification (Type_Name, Enum_Prefix);
            Put (" is");
            New_Line;
            Put
             ("Index : constant Natural := (if Reader.Is_String_Value then");
            New_Line;
            Put (Type_Name);
            Put ("_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)");
            Put ("else 0);");
            New_Line;
            Put ("begin");
            New_Line;
            Put ("if Index > 0 then");
            New_Line;
            Put ("Value := ");
            Put (Enum_Prefix);
            Put (Type_Name);
            Put ("'Val (Index - 1);");
            New_Line;
            Put ("Reader.Read_Next;");
            New_Line;
            Put ("else");
            New_Line;
            Put ("Success := False;");
            New_Line;
            Put ("end if;");
            New_Line;

            Put ("end Input_");
            Put (Type_Name);
            Put (";");
            New_Line;
            New_Line;
         end if;
      end Write_Enum_Body;

      ------------------------------
      -- Write_Enum_Specification --
      ------------------------------

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

            Write_Input_Specification (Type_Name, Enum_Prefix);
            Put (";");
            New_Line;
            New_Line;
         end if;
      end Write_Enum_Specification;

      procedure Write_Variant_Hash
        (Name     : VSS.Strings.Virtual_String;
         Property : VSS.Strings.Virtual_String;
         Schema   : Schema_Access;
         Optional : Boolean)
      is
         pragma Unreferenced (Optional);

         Type_Name : VSS.Strings.Virtual_String := Ref_To_Type_Name (Name);
      begin
         if not Property.Is_Empty then
            Type_Name.Append ("_");
            Type_Name.Append (Property);
         end if;

         Put ("package ");
         Put (Type_Name);
         Put ("_Minimal_Perfect_Hash is new Minimal_Perfect_Hash ([");

         for Index in 1 .. Schema.Any_Of.Last_Index loop
            declare
               Item    : constant Schema_Access :=
                 Schema.Any_Of.Element (Index);

               Variant : constant VSS.Strings.Virtual_String :=
                 Variant_Name (Map, Item);
            begin
               if Index > 1 then
                  Put (", ");
               end if;
               Put ("""");
               Put (Variant);
               Put ("""");
            end;
         end loop;

         Put ("]);");
         New_Line;
         New_Line;
      end Write_Variant_Hash;

   begin
      Print_Vector (Header);
      Put ("with VSS.JSON.Pull_Readers;");
      New_Line;
      New_Line;
      Put ("package ");
      Put (Root_Package);
      Put (".Inputs is");
      New_Line;
      New_Line;

      Each_Enumeration_Type
        (Map, Optional_Types, Write_Enum_Specification'Access);

      for Cursor in Map.Iterate loop
         Write_Named_Type
           (Map, Enum_Package,
            JSON_Schema.Readers.Schema_Maps.Key (Cursor),
            JSON_Schema.Readers.Schema_Maps.Element (Cursor),
            Specification,
            Holders);
      end loop;

      Put ("end ");
      Put (Root_Package);
      Put (".Inputs;");
      New_Line;

      Each_Union_Type (Map, Optional_Types, Count_Union_Types'Access);

      Print_Vector (Header);
      Put ("pragma Ada_2022;");
      New_Line;
      Put ("with Minimal_Perfect_Hash;");
      New_Line;

      if Union_Types_Count > 0 then
         Put ("with VSS.JSON.Pull_Readers.Look_Ahead;");
         New_Line;
      end if;

      New_Line;
      Put ("package body ");
      Put (Root_Package);
      Put (".Inputs is");
      New_Line;
      Put ("pragma Style_Checks (Off);");
      New_Line;
      Put ("use type VSS.JSON.JSON_Number_Kind;"); New_Line;
      Put ("use type VSS.Strings.Virtual_String;"); New_Line;
      New_Line;

      Put ("procedure Input_Any_Value"); New_Line;
      Put ("  (Reader  : in out ");
      Put ("VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;"); New_Line;
      Put ("   Value   : out Any_Value'Class;"); New_Line;
      Put ("   Success : in out Boolean) is"); New_Line;
      Put ("use type VSS.JSON.Pull_Readers.JSON_Event_Kind;"); New_Line;
      Put ("begin"); New_Line;
      Put ("case Reader.Event_Kind is"); New_Line;
      Put ("when VSS.JSON.Pull_Readers.Start_Array =>"); New_Line;
      Put ("Value.Append ((Kind => VSS.JSON.Events.Start_Array));"); New_Line;
      Put ("Reader.Read_Next;"); New_Line;
      Put ("while Success and Reader.Event_Kind /= ");
      Put ("VSS.JSON.Pull_Readers.End_Array loop"); New_Line;
      Put ("Input_Any_Value (Reader, Value, Success);"); New_Line;
      Put ("end loop;"); New_Line;
      Put ("Value.Append ((Kind => VSS.JSON.Events.End_Array));"); New_Line;
      Put ("when VSS.JSON.Pull_Readers.Start_Object =>"); New_Line;
      Put ("Value.Append ((Kind => VSS.JSON.Events.Start_Object));"); New_Line;
      Put ("Reader.Read_Next;"); New_Line;
      Put ("while Success and Reader.Event_Kind = ");
      Put ("VSS.JSON.Pull_Readers.Key_Name loop"); New_Line;
      Put ("Value.Append ((VSS.JSON.Events.Key_Name, ");
      Put ("Reader.Key_Name));"); New_Line;
      Put ("Reader.Read_Next;"); New_Line;
      Put ("Input_Any_Value (Reader, Value, Success);"); New_Line;
      Put ("end loop;"); New_Line;
      Put ("Value.Append ((Kind => VSS.JSON.Events.End_Object));"); New_Line;
      Put ("when VSS.JSON.Pull_Readers.String_Value =>"); New_Line;
      Put ("Value.Append ((VSS.JSON.Events.String_Value, ");
      Put ("Reader.String_Value));"); New_Line;
      Put ("when VSS.JSON.Pull_Readers.Number_Value =>"); New_Line;
      Put ("Value.Append ((VSS.JSON.Events.Number_Value, ");
      Put ("Reader.Number_Value));"); New_Line;
      Put ("when VSS.JSON.Pull_Readers.Boolean_Value =>"); New_Line;
      Put ("Value.Append ((VSS.JSON.Events.Boolean_Value, ");
      Put ("Reader.Boolean_Value));"); New_Line;
      Put ("when VSS.JSON.Pull_Readers.Null_Value =>"); New_Line;
      Put ("Value.Append ((Kind => VSS.JSON.Events.Null_Value));"); New_Line;
      Put ("when others =>"); New_Line;
      Put ("Success := False;"); New_Line;
      Put ("end case;"); New_Line;
      Put ("if Success then"); New_Line;
      Put ("Reader.Read_Next;"); New_Line;
      Put ("end if;"); New_Line;
      Put ("end Input_Any_Value;");
      New_Line;
      New_Line;

      Each_Enumeration_Type (Map, Optional_Types, Write_Enum_Body'Access);
      Each_Union_Type (Map, Optional_Types, Write_Variant_Hash'Access);

      for Cursor in Map.Iterate loop
         Write_Named_Type
           (Map, Enum_Package,
            JSON_Schema.Readers.Schema_Maps.Key (Cursor),
            JSON_Schema.Readers.Schema_Maps.Element (Cursor),
            Implemenetation,
            Holders);
      end loop;

      Put ("end ");
      Put (Root_Package);
      Put (".Inputs;");
      New_Line;
   end Generate_Readers;

   ---------
   -- Put --
   ---------

   procedure Put (Value : Integer) is
   begin
      Put
        (VSS.Strings.Conversions.To_Virtual_String (Integer'Image (Value)));
   end Put;

   --------------------------
   -- Write_Anonymous_Type --
   --------------------------

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map;
      Holders        : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;

      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property);

      -----------------------------
      -- Anonymous_Schema_Reader --
      -----------------------------

      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type (Enclosing_Type, Property, Map, Holders);
      end Anonymous_Schema_Reader;

      Schema : Schema_Access renames Property.Schema;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Enclosing_Type)
        & "_" & Property.Name;
   begin
      Write_Input_Specification (Type_Name, "");
      Put (" is");
      New_Line;

      --  Write Input procedures for anonymous schemas
      Each_Anonymous_Schema (Map, Schema, Anonymous_Schema_Reader'Access);

      Put ("begin");
      New_Line;

      if not Schema.All_Of.Is_Empty or not Schema.Properties.Is_Empty then
         Write_Object_Reader
           (Map, Enclosing_Type, "_" & Property.Name, Schema, Holders);
      else
         Put ("Input_Any_Value (Reader, Value, Success);");
         New_Line;
      end if;

      Put ("end Input_");
      Put (Type_Name);
      Put (";");
      New_Line;
      New_Line;
   end Write_Anonymous_Type;

   ----------------
   -- Write_Hash --
   ----------------

   procedure Write_Hash
     (Map       : JSON_Schema.Readers.Schema_Map;
      Type_Name : VSS.Strings.Virtual_String;
      Schema    : Schema_Access)
   is
      procedure Find_Any_Property
        (Property : JSON_Schema.Property;
         Ignore   : Boolean);
      --  Check if any property exist

      procedure Write_Quoted_Property_Name
        (Property : JSON_Schema.Property;
         Ignore   : Boolean);

      First : Boolean := True;

      -----------------------
      -- Find_Any_Property --
      -----------------------

      procedure Find_Any_Property
        (Property : JSON_Schema.Property;
         Ignore   : Boolean)
      is
         pragma Unreferenced (Property);
      begin
         First := False;
      end Find_Any_Property;

      --------------------------------
      -- Write_Quoted_Property_Name --
      --------------------------------

      procedure Write_Quoted_Property_Name
        (Property : JSON_Schema.Property;
         Ignore   : Boolean) is
      begin
         if First then
            First := False;
         else
            Put (", ");
         end if;

         Put ("""");
         Put (Property.Name);
         Put ("""");
      end Write_Quoted_Property_Name;

   begin
      Writers.Each_Property (Map, Schema, Find_Any_Property'Access);

      if First then
         --  We didn't find any nested properties for some reason.
         --  Skip Minimal_Perfect_Hash instantiation.
         return;
      else
         First := True;
      end if;

      Put ("package ");
      Put (Type_Name);
      Put ("_Minimal_Perfect_Hash is new Minimal_Perfect_Hash ([");

      Writers.Each_Property
        (Map, Schema, Write_Quoted_Property_Name'Access);
      Put ("]);");
      New_Line;
      New_Line;
   end Write_Hash;

   -------------------------------
   -- Write_Input_Specification --
   -------------------------------

   procedure Write_Input_Specification
     (Type_Name : VSS.Strings.Virtual_String;
      Prefix    : VSS.Strings.Virtual_String) is
   begin
      Put ("procedure Input_");
      Put (Type_Name);
      New_Line;
      Put ("(Reader : in out ");
      Put ("VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;");
      New_Line;
      Put ("Value : out ");
      Put (Prefix);
      Put (Type_Name);
      Put (";");
      New_Line;
      Put ("Success : in out Boolean)");
   end Write_Input_Specification;

   ----------------------
   -- Write_Named_Type --
   ----------------------

   procedure Write_Named_Type
     (Map          : JSON_Schema.Readers.Schema_Map;
      Enum_Package : VSS.Strings.Virtual_String;
      Name         : VSS.Strings.Virtual_String;
      Schema       : Schema_Access;
      Kind         : Declaration_Kind;
      Holders      : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name);

      procedure Hash_For_Anonymous_Schema (Property : JSON_Schema.Property);
      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property);

      -----------------------------
      -- Anonymous_Schema_Reader --
      -----------------------------

      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type (Name, Property, Map, Holders);
      end Anonymous_Schema_Reader;

      -------------------------------
      -- Hash_For_Anonymous_Schema --
      -------------------------------

      procedure Hash_For_Anonymous_Schema (Property : JSON_Schema.Property) is
         use type VSS.Strings.Virtual_String;
      begin
         Write_Hash (Map, Type_Name & "_" & Property.Name, Property.Schema);
      end Hash_For_Anonymous_Schema;

      Enum_Prefix : constant VSS.Strings.Virtual_String :=
        (if Enum_Package.Is_Empty then VSS.Strings.Empty_Virtual_String
         else Enum_Package & ".");

   begin
      if not Schema.Enum.Is_Empty then
         return;
      end if;

      if Kind = Implemenetation then
         Write_Hash (Map, Type_Name, Schema);
         Each_Anonymous_Schema (Map, Schema, Hash_For_Anonymous_Schema'Access);
      end if;

      Write_Input_Specification (Type_Name, "");

      if Kind = Specification then
         Put (";");
         New_Line;
         New_Line;
         return;
      end if;

      Put (" is");
      New_Line;

      --  Write Input procedures for anonymous schemas
      Each_Anonymous_Schema (Map, Schema, Anonymous_Schema_Reader'Access);

      if not Schema.Any_Of.Is_Empty then
         --  Declaration items for union type reader
         Put ("use all type VSS.JSON.Pull_Readers.JSON_Event_Kind;"); New_Line;
         New_Line;
         Put ("Look_Ahead : ");
         Put ("VSS.JSON.Pull_Readers.Look_Ahead.JSON_Look_Ahead_Reader");
         Put (" (Reader'Access);"); New_Line;
         New_Line;
         Put ("Variant_Key : constant VSS.Strings.Virtual_String :=");
         Put (" ""kind"";");
         New_Line;
      end if;

      Put ("begin");
      New_Line;

      if not Schema.All_Of.Is_Empty or not Schema.Properties.Is_Empty then
         Write_Object_Reader (Map, Name, "", Schema, Holders);
      elsif not Schema.Any_Of.Is_Empty then
         Write_Union_Reader (Map, Name, Schema, Enum_Package);
      else
         Put ("Input_Any_Value (Reader, Value, Success);");
         New_Line;
      end if;

      Put ("end Input_");
      Put (Type_Name);
      Put (";");
      New_Line;
      New_Line;
   end Write_Named_Type;

   -------------------------
   -- Write_Object_Reader --
   -------------------------

   procedure Write_Object_Reader
     (Map     : JSON_Schema.Readers.Schema_Map;
      Name    : VSS.Strings.Virtual_String;
      Suffix  : VSS.Strings.Virtual_String;
      Schema  : Schema_Access;
      Holders : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;

      procedure Write_When_Clause
        (Property : JSON_Schema.Property;
         Required : Boolean);

      Index : Positive := 1;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name) & Suffix;

      -----------------------
      -- Write_When_Clause --
      -----------------------

      procedure Write_When_Clause
        (Property : JSON_Schema.Property;
         Required : Boolean) is
      begin
         Put ("   when");
         Put (Index);
         Put (" =>  --  ");
         Put (Property.Name);
         New_Line;

         Write_Record_Component (Name, Map, Property, Required, Holders);

         Index := Index + 1;
      end Write_When_Clause;
   begin
      Put ("if Success and Reader.Is_Start_Object then"); New_Line;
      Put ("Reader.Read_Next;"); New_Line;
      Put ("else"); New_Line;
      Put ("Success := False;"); New_Line;
      Put ("end if;"); New_Line;
      New_Line;
      Put ("while Success and not Reader.Is_End_Object loop"); New_Line;
      Put ("if Reader.Is_Key_Name then"); New_Line;
      Put ("declare"); New_Line;
      Put ("Index : constant Natural :="); New_Line;
      Put (Type_Name);
      Put ("_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);"); New_Line;
      Put ("begin"); New_Line;
      Put ("Reader.Read_Next;"); New_Line;
      New_Line;
      Put ("case Index is"); New_Line;

      Writers.Each_Property (Map, Schema, Write_When_Clause'Access);

      Put ("when others =>"); New_Line;
      Put ("Reader.Skip_Current_Value;"); New_Line;
      Put ("end case;"); New_Line;
      Put ("end;"); New_Line;
      Put ("else"); New_Line;
      Put ("   Success := False;"); New_Line;
      Put ("end if;"); New_Line;
      Put ("end loop;"); New_Line;
      New_Line;
      Put ("if Success then"); New_Line;
      Put ("Reader.Read_Next;  --  skip End_Object"); New_Line;
      Put ("end if;"); New_Line;
   end Write_Object_Reader;

   ----------------------------
   -- Write_Record_Component --
   ----------------------------

   procedure Write_Record_Component
     (Name     : VSS.Strings.Virtual_String;
      Map      : JSON_Schema.Readers.Schema_Map;
      Property : JSON_Schema.Property;
      Required : Boolean;
      Holders  : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;
      use all type VSS.JSON.Events.JSON_Event_Kind;

      procedure Write_Value
        (Field_Name  : VSS.Strings.Virtual_String;
         Type_Name   : VSS.Strings.Virtual_String);

      -----------------
      -- Write_Value --
      -----------------

      procedure Write_Value
        (Field_Name  : VSS.Strings.Virtual_String;
         Type_Name   : VSS.Strings.Virtual_String) is
      begin
         if Type_Name = "Any_Object" then
            Write_Value (Field_Name, "Any_Value");
         elsif Type_Name = "Virtual_String" then
            Put ("if Reader.Is_String_Value then"); New_Line;
            Put (Field_Name);
            Put (" := Reader.String_Value;"); New_Line;
            Put ("Reader.Read_Next;"); New_Line;
            Put ("else"); New_Line;
            Put ("Success := False;"); New_Line;
            Put ("end if;"); New_Line;
         elsif Type_Name = "Integer" then
            Put ("if Reader.Is_Number_Value and then ");
            Put ("Reader.Number_Value.Kind = VSS.JSON.JSON_Integer then");
            New_Line;

            Put (Field_Name);
            Put (" := Integer (Reader.Number_Value.Integer_Value);"); New_Line;
            Put ("Reader.Read_Next;"); New_Line;
            Put ("else"); New_Line;
            Put ("Success := False;"); New_Line;
            Put ("end if;"); New_Line;
         elsif Type_Name = "Float" then
            Put ("if Reader.Is_Number_Value then"); New_Line;
            Put ("if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer then");
            New_Line;

            Put (Field_Name);
            Put (" := Float (Reader.Number_Value.Integer_Value);"); New_Line;
            Put ("elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float then");
            New_Line;
            Put (Field_Name);
            Put (" := Float (Reader.Number_Value.Float_Value);"); New_Line;
            Put ("else"); New_Line;
            Put ("Success := False;"); New_Line;
            Put ("end if;"); New_Line;
            Put ("Reader.Read_Next;"); New_Line;
            Put ("else"); New_Line;
            Put ("Success := False;"); New_Line;
            Put ("end if;"); New_Line;
         elsif Type_Name = "Boolean" then
            Put ("if Reader.Is_Boolean_Value then"); New_Line;
            Put (Field_Name);
            Put (" := Reader.Boolean_Value;"); New_Line;
            Put ("Reader.Read_Next;"); New_Line;
            Put ("else"); New_Line;
            Put ("Success := False;"); New_Line;
            Put ("end if;"); New_Line;
         elsif Type_Name.Ends_With ("_Vector") then
            declare
               Item_Type   : VSS.Strings.Virtual_String;
               Type_Prefix : VSS.Strings.Virtual_String;
            begin
               Get_Element_Type
                 (Map, Property.Schema, Item_Type, Type_Prefix);

               Put ("if Success and Reader.Is_Start_Array then"); New_Line;
               Put ("Reader.Read_Next;"); New_Line;
               Put ("while Success and not Reader.Is_End_Array loop");
               New_Line;
               Put ("declare"); New_Line;
               Put ("Item : ");
               Put (Type_Prefix);
               Put (Item_Type);
               Put (";"); New_Line;
               Put ("begin"); New_Line;
               Write_Value ("Item", Item_Type);
               Put (Field_Name);
               Put (".Append (Item);"); New_Line;
               Put ("end;"); New_Line;
               Put ("end loop;"); New_Line;
               Put ("if Success then"); New_Line;
               Put ("Reader.Read_Next;  --  skip End_Array"); New_Line;
               Put ("end if;"); New_Line;
               Put ("else"); New_Line;
               Put ("Success := False;"); New_Line;
               Put ("end if;"); New_Line;
            end;
         elsif Type_Name = "Integer_Or_String" then
            --  Turn Value into an integer to simplify `elsif` part
            Put (Field_Name);
            Put (" := (False, Integer => <>);"); New_Line;
            Put ("if Reader.Is_String_Value then"); New_Line;
            Put (Field_Name);
            Put (" := (True, Reader.String_Value);"); New_Line;
            Put ("Reader.Read_Next;"); New_Line;
            Put ("els");
            Write_Value (Field_Name & ".Integer", "Integer");
         else
            Put ("Input_");
            Put (Type_Name);
            Put (" (Reader, ");
            Put (Field_Name);
            Put (", Success);");
            New_Line;
         end if;
      end Write_Value;

      Fallback : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name) & "_" & Property.Name;

      Field_Name : constant VSS.Strings.Virtual_String :=
        Escape_Keywords (Property.Name);

      Is_Holder   : constant Boolean :=
        Writers.Is_Holder_Field (Name, Property.Name, Holders);

      Element : constant VSS.Strings.Virtual_String := ".Element";

      Suffix : constant VSS.Strings.Virtual_String :=
        (if Is_Holder then Element else VSS.Strings.Empty_Virtual_String);

      Type_Name   : VSS.Strings.Virtual_String;
      Type_Prefix : VSS.Strings.Virtual_String;
   begin
      Get_Field_Type
        (Map, Property.Schema, True, Fallback, Type_Name, Type_Prefix);

      if Property.Schema.Enum.Length = 1 then
         --  Check constant property (single item enum)
         Put ("if Reader.Is_String_Value and then Reader.String_Value = """);
         Put (Property.Schema.Enum.Element (1));
         Put (""" then");
         New_Line;
         Put ("Reader.Read_Next;");
         New_Line;
         Put ("else");
         New_Line;
         Put ("Success := False;");
         New_Line;
         Put ("end if;");
         New_Line;
      elsif not Property.Schema.Const.Is_Empty then
         --  Write constant property

         --  Only string constant is supported for now.
         pragma Assert
          (Property.Schema.Const.First_Element.Kind = String_Value);

         Put ("if Reader.Is_String_Value and then Reader.String_Value = """);
         Put (Property.Schema.Const.First_Element.String_Value);
         Put (""" then");
         New_Line;
         Put ("Reader.Read_Next;");
         New_Line;
         Put ("else");
         New_Line;
         Put ("Success := False;");
         New_Line;
         Put ("end if;");
         New_Line;
      elsif not Required and Type_Name = "Virtual_String" then
         Write_Value ("Value." & Field_Name, Type_Name);
      elsif not Required and Type_Name = "Virtual_String_Vector" then
         Write_Value ("Value." & Field_Name, Type_Name);
      elsif not Required and Type_Name.Ends_With ("_Vector") then
         Write_Value ("Value." & Field_Name, Type_Name);
      elsif not Required and Type_Name = "Boolean" then
         Write_Value ("Value." & Field_Name, Type_Name);
      elsif not Required and
         (Property.Schema.Kind.Last_Index = 7 or
           Property.Schema.Additional_Properties /= null)
      then
         Write_Value ("Value." & Field_Name, Type_Name);
      elsif Required then
         Write_Value ("Value." & Field_Name & Suffix, Type_Name);
      else
         Put ("Value.");
         Put (Field_Name);
         Put (" := (Is_Set => True, Value => <>);");
         New_Line;
         Write_Value ("Value." & Field_Name & ".Value", Type_Name);
      end if;
   end Write_Record_Component;

   ------------------------
   -- Write_Union_Reader --
   ------------------------

   procedure Write_Union_Reader
     (Map          : JSON_Schema.Readers.Schema_Map;
      Name         : VSS.Strings.Virtual_String;
      Schema       : Schema_Access;
      Enum_Package : VSS.Strings.Virtual_String)
   is
      use type VSS.Strings.Virtual_String;

      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name);

      Enum_Prefix : constant VSS.Strings.Virtual_String :=
        (if Enum_Package.Is_Empty then VSS.Strings.Empty_Virtual_String
         else Enum_Package & ".");
   begin
      Put ("if Success and Look_Ahead.Is_Start_Object then"); New_Line;
      Put ("Look_Ahead.Read_Next;"); New_Line;
      Put ("else"); New_Line;
      Put ("Success := False;"); New_Line;
      Put ("end if;"); New_Line;
      New_Line;
      Put ("while Success and not Look_Ahead.Is_End_Object loop"); New_Line;
      Put ("if not Look_Ahead.Is_Key_Name then"); New_Line;
      Put ("Success := False;"); New_Line;
      Put ("elsif Look_Ahead.Key_Name /= Variant_Key then"); New_Line;
      Put ("Look_Ahead.Skip_Current_Value;"); New_Line;
      Put ("Success := not Look_Ahead.Is_End_Object;"); New_Line;
      Put ("elsif Look_Ahead.Read_Next = String_Value then"); New_Line;
      Put ("declare"); New_Line;
      Put ("Index : constant Natural :="); New_Line;
      Put (Type_Name);
      Put ("_Minimal_Perfect_Hash.Get_Index"); New_Line;
      Put ("(Look_Ahead.String_Value);"); New_Line;
      Put ("begin"); New_Line;
      Put ("Look_Ahead.Rewind;"); New_Line;
      New_Line;
      Put ("case Index is"); New_Line;

      for Index in 1 .. Schema.Any_Of.Last_Index loop
         declare
            Item    : constant Schema_Access :=
              Schema.Any_Of.Element (Index);

            Variant : constant VSS.Strings.Virtual_String :=
              Escape_Keywords (Variant_Name (Map, Item));

            Fallback : constant VSS.Strings.Virtual_String :=
              Ref_To_Type_Name (Name) & "_" & Variant;

            Type_Name   : VSS.Strings.Virtual_String;
            Type_Prefix : VSS.Strings.Virtual_String;
         begin
            Get_Field_Type
              (Map, Item, True, Fallback, Type_Name, Type_Prefix);
            Put ("when ");
            Put (Index);
            Put (" =>  --  ");
            Put (Variant);
            New_Line;
            Put ("Value.Union := (Kind => ");
            Put (Enum_Prefix);
            Put (Variant);
            Put (", others => <>);");
            New_Line;
            Put ("Input_");
            Put (Type_Name);
            Put (" (Look_Ahead, Value.Union.");
            Put (Variant);
            Put (", Success);");
            New_Line;
         end;
      end loop;

      Put ("when others =>"); New_Line;
      Put ("Success := False;"); New_Line;
      Put ("end case;"); New_Line;
      New_Line;
      Put ("return;"); New_Line;
      Put ("end;"); New_Line;
      Put ("else"); New_Line;
      Put ("Success := False;"); New_Line;
      Put ("end if;"); New_Line;
      Put ("end loop;"); New_Line;
      New_Line;
      Put ("if Success then"); New_Line;
      Put ("Look_Ahead.Read_Next;  --  skip End_Object"); New_Line;
      Put ("Success := False;"); New_Line;
      Put ("end if;"); New_Line;
   end Write_Union_Reader;

end JSON_Schema.Writers.Inputs;
