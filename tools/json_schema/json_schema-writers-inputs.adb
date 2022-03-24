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

with VSS.Strings.Conversions;

package body JSON_Schema.Writers.Inputs is

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

   procedure Write_Hash
     (Map       : JSON_Schema.Readers.Schema_Map;
      Type_Name : VSS.Strings.Virtual_String;
      Schema    : Schema_Access);

   procedure Write_Named_Type
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access;
      Kind   : Declaration_Kind);

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map);

   procedure Write_Object_Reader
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Suffix : VSS.Strings.Virtual_String;
      Schema : Schema_Access);

   procedure Write_Input_Specification
     (Type_Name : VSS.Strings.Virtual_String;
      Prefix    : VSS.Strings.Virtual_String);

   procedure Write_Record_Component
     (Name     : VSS.Strings.Virtual_String;
      Map      : JSON_Schema.Readers.Schema_Map;
      Property : JSON_Schema.Property;
      Required : Boolean);

   ----------------------
   -- Generate_Readers --
   ----------------------

   procedure Generate_Readers
     (Map            : JSON_Schema.Readers.Schema_Map;
      Optional_Types : String_Sets.Set) is
   begin
      Put ("with Interfaces;");
      New_Line;
      Put ("with VSS.JSON.Pull_Readers;");
      New_Line;
      New_Line;
      Put ("package ");
      Put (Package_Name);
      Put (".Inputs is");
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
      Put (Package_Name);
      Put (".Inputs;");
      New_Line;

      Put ("pragma Ada_2022;");
      New_Line;
      Put ("with Minimal_Perfect_Hash;");
      New_Line;
      New_Line;
      Put ("package body ");
      Put (Package_Name);
      Put (".Inputs is");
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

      for Cursor in Map.Iterate loop
         Write_Named_Type
           (Map,
            JSON_Schema.Readers.Schema_Maps.Key (Cursor),
            JSON_Schema.Readers.Schema_Maps.Element (Cursor),
            Implemenetation);
      end loop;

      Put ("end ");
      Put (Package_Name);
      Put (".Inputs;");
      New_Line;
   end Generate_Readers;

   --------------------------
   -- Write_Anonymous_Type --
   --------------------------

   procedure Write_Anonymous_Type
     (Enclosing_Type : VSS.Strings.Virtual_String;
      Property       : JSON_Schema.Property;
      Map            : JSON_Schema.Readers.Schema_Map)
   is
      use type VSS.Strings.Virtual_String;

      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property);

      -----------------------------
      -- Anonymous_Schema_Reader --
      -----------------------------

      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type (Enclosing_Type, Property, Map);
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
      Each_Anonymous_Schema (Schema, Anonymous_Schema_Reader'Access);

      Put ("begin");
      New_Line;

      if not Schema.All_Of.Is_Empty or not Schema.Properties.Is_Empty then
         Write_Object_Reader
           (Map, Enclosing_Type, "_" & Property.Name, Schema);
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

         Write_Input_Specification (Type_Name, "Enum.");
         Put (" is");
         New_Line;
         Put ("Index : constant Natural := (if Reader.Is_String_Value then");
         New_Line;
         Put (Type_Name);
         Put ("_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)");
         Put ("else 0);");
         New_Line;
         Put ("begin");
         New_Line;
         Put ("if Index > 0 then");
         New_Line;
         Put ("Value := Enum.");
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

         Write_Input_Specification (Type_Name, "Enum.");
         Put (";");
         New_Line;
         New_Line;
      end if;
   end Write_Enum_Specification;

   ----------------
   -- Write_Hash --
   ----------------

   procedure Write_Hash
     (Map       : JSON_Schema.Readers.Schema_Map;
      Type_Name : VSS.Strings.Virtual_String;
      Schema    : Schema_Access)
   is
      procedure Write_Quoted_Property_Name
        (Property : JSON_Schema.Property;
         Ignore   : Boolean);

      First : Boolean := True;

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
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access;
      Kind   : Declaration_Kind)
   is
      Type_Name : constant VSS.Strings.Virtual_String :=
        Ref_To_Type_Name (Name);

      procedure Hash_For_Anonymous_Schema (Property : JSON_Schema.Property);
      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property);

      -----------------------------
      -- Anonymous_Schema_Reader --
      -----------------------------

      procedure Anonymous_Schema_Reader (Property : JSON_Schema.Property) is
      begin
         Write_Anonymous_Type (Name, Property, Map);
      end Anonymous_Schema_Reader;

      -------------------------------
      -- Hash_For_Anonymous_Schema --
      -------------------------------

      procedure Hash_For_Anonymous_Schema (Property : JSON_Schema.Property) is
         use type VSS.Strings.Virtual_String;
      begin
         Write_Hash (Map, Type_Name & "_" & Property.Name, Property.Schema);
      end Hash_For_Anonymous_Schema;

   begin
      if not Schema.Enum.Is_Empty then
         return;
      end if;

      if Kind = Implemenetation then
         Write_Hash (Map, Type_Name, Schema);
         Each_Anonymous_Schema (Schema, Hash_For_Anonymous_Schema'Access);
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
      Each_Anonymous_Schema (Schema, Anonymous_Schema_Reader'Access);

      Put ("begin");
      New_Line;

      if not Schema.All_Of.Is_Empty or not Schema.Properties.Is_Empty then
         Write_Object_Reader (Map, Name, "", Schema);
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
     (Map    : JSON_Schema.Readers.Schema_Map;
      Name   : VSS.Strings.Virtual_String;
      Suffix : VSS.Strings.Virtual_String;
      Schema : Schema_Access)
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
         Put
           (VSS.Strings.Conversions.To_Virtual_String (Integer'Image (Index)));
         Put (" =>  --  ");
         Put (Property.Name);
         New_Line;

         Write_Record_Component (Name, Map, Property, Required);

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
      Put ("Success := False;"); New_Line;
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
      Required : Boolean)
   is
      use type VSS.Strings.Virtual_String;

      procedure Write_Value
        (Field_Name  : VSS.Strings.Virtual_String;
         Type_Name   : VSS.Strings.Virtual_String;
         Type_Prefix : VSS.Strings.Virtual_String);

      -----------------
      -- Write_Value --
      -----------------

      procedure Write_Value
        (Field_Name  : VSS.Strings.Virtual_String;
         Type_Name   : VSS.Strings.Virtual_String;
         Type_Prefix : VSS.Strings.Virtual_String)
      is
         pragma Unreferenced (Type_Prefix);
      begin
         if Type_Name = "Any_Object" then
            Write_Value (Field_Name, "Any_Value", "");
         elsif Type_Name = "Virtual_String" then
            Put ("if Reader.Is_String_Value then "); New_Line;
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
            Put ("if Reader.Is_Number_Value then "); New_Line;
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
            Put ("if Reader.Is_Boolean_Value then "); New_Line;
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
               if Type_Name = "Virtual_String_Vector" then
                  Item_Type := "Virtual_String";
                  Type_Prefix := "VSS.Strings.";
               else
                  Get_Element_Type
                    (Map, Property.Schema, Item_Type, Type_Prefix);
               end if;

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
               Write_Value ("Item", Item_Type, Type_Prefix);
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
            Put ("if ");
            Put (Field_Name);
            Put (".Is_String then");
            New_Line;
            Write_Value
              (Field_Name & ".String", "Virtual_String", "VSS.Strings.");
            Put ("else");
            New_Line;
            Write_Value (Field_Name & ".Integer", "Integer", "");
            Put ("end if;");
            New_Line;
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

      Type_Name   : VSS.Strings.Virtual_String;
      Type_Prefix : VSS.Strings.Virtual_String;
   begin
      Get_Field_Type
        (Map, Property.Schema, True, Fallback, Type_Name, Type_Prefix);

      if Property.Schema.Enum.Length = 1 then
         --  Check constant property
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
      elsif not Required and Type_Name = "Virtual_String" then
         Write_Value ("Value." & Field_Name, Type_Name, Type_Prefix);
      elsif not Required and Type_Name = "Virtual_String_Vector" then
         Write_Value ("Value." & Field_Name, Type_Name, "VSS.Strings.");
      elsif not Required and Type_Name.Ends_With ("_Vector") then
         Write_Value ("Value." & Field_Name, Type_Name, Type_Prefix);
      elsif not Required and Type_Name = "Boolean" then
         Write_Value ("Value." & Field_Name, Type_Name, Type_Prefix);
      elsif (not Required and Property.Schema.Kind.Last_Index = 7)
        or
          (not Required
           and then not Property.Schema.Additional_Properties.Is_Boolean
           and then Property.Schema.Additional_Properties.Schema /= null)
      then
         Write_Value ("Value." & Field_Name, Type_Name, Type_Prefix);
      elsif Required then
         Write_Value ("Value." & Field_Name, Type_Name, Type_Prefix);
      else
         Put ("Value.");
         Put (Field_Name);
         Put (" := (Is_Set => True, Value => <>);");
         New_Line;
         Write_Value
           ("Value." & Field_Name & ".Value", Type_Name, Type_Prefix);
      end if;
   end Write_Record_Component;

end JSON_Schema.Writers.Inputs;