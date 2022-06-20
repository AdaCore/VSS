--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body JSON_Schema.Readers is

   function To_Simple_Type (Value : VSS.Strings.Virtual_String)
     return Definitions.Simple_Types;
   --  Cast string to Simple_Types

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Schema_Access;
      Other   : out Schema_Map;
      Prefix  : VSS.Strings.Virtual_String;
      Version : Schema_Version);
   --  Parse single object from JSON as a schema into the Value and include
   --  all sub-schemas into Other map. Use Prefix for subschema names.

   Version_URL : constant array (Schema_Version) of URI :=
     (Draft_4 => "http://json-schema.org/draft-04/schema#",
      Draft_6 => "http://json-schema.org/draft-06/schema#",
      Draft_7 => "http://json-schema.org/draft-07/schema#");

   function Id_Key (Value : Schema_Version) return VSS.Strings.Virtual_String;
   --  Return `$id` or `id` depending on schema version

   procedure Read_Any_JSON_Value
     (Reader : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Result : in out JSON_Event_Vectors.Vector'Class);

   ------------
   -- Id_Key --
   ------------

   function Id_Key (Value : Schema_Version)
     return VSS.Strings.Virtual_String is
   begin
      if Value = Draft_4 then
         return "id";
      else
         return "$id";
      end if;
   end Id_Key;

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Schema_Access;
      Other   : out Schema_Map;
      Prefix  : VSS.Strings.Virtual_String;
      Version : Schema_Version)
   is
      use type VSS.Strings.Virtual_String;
      Key    : VSS.Strings.Virtual_String;
      Nested : Schema_Access;

      Current_Version : Schema_Version := Version;

      Exclusive_Maximum, Exclusive_Minimum : Boolean := False;
      --  In Draft 4 these fields are boolean. Read them and convert latter.
   begin
      Value := new JSON_Schema.Schema;

      if Reader.Is_Boolean_Value then
         --  `True` schema equals to `{}`, `False` schema equals to `not {}`

         if not Reader.Boolean_Value then
            Value.Negate := new JSON_Schema.Schema;
         end if;

         Reader.Read_Next;
         return;
      end if;

      pragma Assert (Reader.Is_Start_Object);
      Reader.Read_Next;

      while not Reader.At_End and then not Reader.Is_End_Object loop
         pragma Assert (Reader.Is_Key_Name);
         Key := Reader.Key_Name;
         Reader.Read_Next;

         if Key = Id_Key (Current_Version) then
            pragma Assert (Reader.Is_String_Value);
            Value.Id := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "$schema" then
            pragma Assert (Reader.Is_String_Value);
            Value.Schema := Reader.String_Value;
            Reader.Read_Next;

            --  Change Current_Version according to `$schema` value
            for J in Schema_Version loop
               if Version_URL (J) = Value.Schema then
                  Current_Version := J;
                  exit;
               end if;
            end loop;
         elsif Key = "$comment" then
            pragma Assert (Reader.Is_String_Value);
            Value.Comment := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "title" then
            pragma Assert (Reader.Is_String_Value);
            Value.Title := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "description" then
            pragma Assert (Reader.Is_String_Value);
            Value.Description := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "default" then
            Read_Any_JSON_Value (Reader, Value.Default);
         elsif Key = "readOnly" then
            pragma Assert (Reader.Is_Boolean_Value);
            Value.Read_Only := Reader.Boolean_Value;
            Reader.Read_Next;
         elsif Key = "writeOnly" then
            pragma Assert (Reader.Is_Boolean_Value);
            Value.Write_Only := Reader.Boolean_Value;
            Reader.Read_Next;
         elsif Key = "examples" then
            pragma Assert (Reader.Is_Start_Array);
            Read_Any_JSON_Value (Reader, Value.Examples);
         elsif Key = "multipleOf" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Multiple_Of := Reader.Number_Value;
            Reader.Read_Next;
         elsif Key = "maximum" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Maximum := Reader.Number_Value;
            Reader.Read_Next;
         elsif Key = "exclusiveMaximum" then
            if Current_Version = Draft_4 then
               pragma Assert (Reader.Is_Boolean_Value);
               Exclusive_Maximum := Reader.Boolean_Value;
               Reader.Read_Next;
            else
               pragma Assert (Reader.Is_Null_Value);
               Value.Exclusive_Maximum := Reader.Number_Value;
               Reader.Read_Next;
            end if;
         elsif Key = "minimum" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Minimum := Reader.Number_Value;
            Reader.Read_Next;
         elsif Key = "exclusiveMinimum" then
            if Current_Version = Draft_4 then
               pragma Assert (Reader.Is_Boolean_Value);
               Exclusive_Minimum := Reader.Boolean_Value;
               Reader.Read_Next;
            else
               pragma Assert (Reader.Is_Null_Value);
               Value.Exclusive_Minimum := Reader.Number_Value;
               Reader.Read_Next;
            end if;
         elsif Key = "maxLength" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Max_Length := Definitions.Non_Negative_Integer
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "minLength" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Min_Length := Definitions.Non_Negative_Integer_Default_0
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "pattern" then
            pragma Assert (Reader.Is_String_Value);
            Value.Pattern := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "additionalItems" then

            Read (Reader, Value.Additional_Items, Other, Prefix, Version);

         elsif Key = "items" then
            pragma Assert (Reader.Is_Start_Object or Reader.Is_Start_Array);

            if Reader.Is_Start_Array then
               Reader.Read_Next;

               while not Reader.At_End and then not Reader.Is_End_Array loop
                  declare
                     Item : Schema_Access;
                  begin
                     Read (Reader, Item, Other, Prefix, Version);
                     Value.Items.Append (Item);
                  end;
               end loop;

               pragma Assert (Reader.Is_End_Array);
               Reader.Read_Next;
            else
               declare
                  Item : Schema_Access;
               begin
                  Read (Reader, Item, Other, Prefix, Version);
                  Value.Items.Append (Item);
               end;
            end if;

         elsif Key = "maxItems" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Max_Items := Definitions.Non_Negative_Integer
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "minItems" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Min_Items := Definitions.Non_Negative_Integer_Default_0
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "uniqueItems" then
            pragma Assert (Reader.Is_Boolean_Value);
            Value.Unique_Items := Reader.Boolean_Value;
            Reader.Read_Next;
         elsif Key = "contains" then

            Read (Reader, Value.Contains, Other, Prefix, Version);

         elsif Key = "maxProperties" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Max_Properties := Definitions.Non_Negative_Integer
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "minProperties" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Min_Properties := Definitions.Non_Negative_Integer_Default_0
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "required" then
            pragma Assert (Reader.Is_Start_Array);

            Reader.Read_Next;

            while not Reader.At_End and then not Reader.Is_End_Array loop
               pragma Assert (Reader.Is_String_Value);
               Value.Required.Append (Reader.String_Value);
               Reader.Read_Next;
            end loop;

            pragma Assert (Reader.Is_End_Array);
            Reader.Read_Next;

         elsif Key = "additionalProperties" then

            Read (Reader, Value.Additional_Properties, Other, Prefix, Version);

         elsif Key = "properties"
           or else Key =  "patternProperties"
         then
            pragma Assert (Reader.Is_Start_Object);
            Reader.Read_Next;

            while not Reader.At_End and then not Reader.Is_End_Object loop
               declare
                  Item : Property;
               begin
                  pragma Assert (Reader.Is_Key_Name);
                  Item.Name := Reader.Key_Name;
                  Reader.Read_Next;
                  Read (Reader, Nested, Other, Prefix, Version);
                  Item.Schema := Nested;

                  if Key = "properties" then
                     Value.Properties.Append (Item);
                  else
                     Value.Pattern_Properties.Append (Item);  --  Check regexp?
                  end if;
               end;
            end loop;

            pragma Assert (Reader.Is_End_Object);
            Reader.Read_Next;

         elsif Key = "dependencies" then
            pragma Assert (Reader.Is_Start_Object or Reader.Is_String_Value);

            if Reader.Is_String_Value then
               Value.Dependencies := (True, Reader.String_Value);
               Reader.Read_Next;
            else
               Read (Reader, Nested, Other, Prefix, Version);
               Value.Dependencies := (False, Schema => Nested);
            end if;

         elsif Key = "propertyNames" then

            Read (Reader, Value.Property_Names, Other, Prefix, Version);

         elsif Key = "const" then
            Read_Any_JSON_Value (Reader, Value.Const);
         elsif Key = "enum" or  Key = "_enum" then
            pragma Assert (Reader.Is_Start_Array);

            Reader.Read_Next;

            while not Reader.At_End and then not Reader.Is_End_Array loop
               pragma Assert (Reader.Is_String_Value);
               Value.Enum.Append (Reader.String_Value);
               Reader.Read_Next;
            end loop;

            pragma Assert (Reader.Is_End_Array);
            Reader.Read_Next;
         elsif Key = "type" then
            pragma Assert (Reader.Is_Start_Array or Reader.Is_String_Value);

            if Reader.Is_String_Value then
               Value.Kind.Append (To_Simple_Type (Reader.String_Value));
            else
               Reader.Read_Next;

               while not Reader.At_End and then not Reader.Is_End_Array loop
                  pragma Assert (Reader.Is_String_Value);
                  Value.Kind.Append (To_Simple_Type (Reader.String_Value));
                  Reader.Read_Next;
               end loop;

               pragma Assert (Reader.Is_End_Array);
            end if;

            Reader.Read_Next;

         elsif Key = "format" then
            pragma Assert (Reader.Is_String_Value);
            Value.Format := Reader.String_Value;
            Reader.Read_Next;

         elsif Key = "contentMediaType" then
            pragma Assert (Reader.Is_String_Value);
            Value.Content_Media_Type := Reader.String_Value;
            Reader.Read_Next;

         elsif Key = "contentEncoding" then
            pragma Assert (Reader.Is_String_Value);
            Value.Content_Encoding := Reader.String_Value;
            Reader.Read_Next;

         elsif Key = "if" then

            Read (Reader, Value.If_Schema, Other, Prefix, Version);

         elsif Key = "then" then

            Read (Reader, Value.Then_Schema, Other, Prefix, Version);

         elsif Key = "else" then

            Read (Reader, Value.Else_Schema, Other, Prefix, Version);

         elsif Key = "allOf" or else Key = "anyOf" or else Key = "oneOf" then
            pragma Assert (Reader.Is_Start_Array);
            Reader.Read_Next;

            while not Reader.At_End and then not Reader.Is_End_Array loop
               declare
                  Item : Schema_Access;
               begin
                  Read (Reader, Item, Other, Prefix, Version);

                  if Key = "allOf" then
                     Value.All_Of.Append (Item);
                  elsif Key = "anyOf" then
                     Value.Any_Of.Append (Item);
                  else
                     Value.One_Of.Append (Item);
                  end if;
               end;
            end loop;

            pragma Assert (Reader.Is_End_Array);
            Reader.Read_Next;

         elsif Key = "not" then

            Read (Reader, Value.Negate, Other, Prefix, Version);

         elsif Key = "$ref" then
            pragma Assert (Reader.Is_String_Value);
            Value.Ref := Reader.String_Value;
            Reader.Read_Next;

         elsif Key = "default"
           or Key = "enumDescriptions"  --  what's enumDescriptions???
         then
            Reader.Skip_Current_Value;

         else
            --  Any unknown object is a new schema namespace
            pragma Assert (Reader.Is_Start_Object);
            Reader.Read_Next;

            while not Reader.At_End and then not Reader.Is_End_Object loop
               declare
                  Name : constant VSS.Strings.Virtual_String :=
                    Prefix & "/" & Key & "/" & Reader.Key_Name;
                  Item : Schema_Access;
               begin
                  Reader.Read_Next;
                  Read (Reader, Item, Other, Name, Version);
                  Other.Insert (Name, Item);
               end;
            end loop;

            pragma Assert (Reader.Is_End_Object);
            Reader.Read_Next;
         end if;
      end loop;

      pragma Assert (Reader.Is_End_Object);
      Reader.Read_Next;

      if Exclusive_Maximum then
         Value.Exclusive_Maximum := Value.Maximum;
         Value.Maximum := (Kind => VSS.JSON.None);
      end if;

      if Exclusive_Minimum then
         Value.Exclusive_Minimum := Value.Minimum;
         Value.Minimum := (Kind => VSS.JSON.None);
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Schema  : out Schema_Access;
      Other   : out Schema_Map;
      Version : Schema_Version := Schema_Version'Last) is
   begin
      Read (Reader, Schema, Other, "#", Version);
   end Read;

   -------------------------
   -- Read_Any_JSON_Value --
   -------------------------

   procedure Read_Any_JSON_Value
     (Reader : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Result : in out JSON_Event_Vectors.Vector'Class)
   is
      use all type VSS.JSON.Pull_Readers.JSON_Event_Kind;

      function To_Event (Reader : VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
        return VSS.JSON.Events.JSON_Event is
          (case Reader.Event_Kind is
           when Start_Array => (Kind => VSS.JSON.Events.Start_Array),
           when End_Array => (Kind => VSS.JSON.Events.End_Array),
           when Start_Object => (Kind => VSS.JSON.Events.Start_Object),
           when End_Object => (Kind => VSS.JSON.Events.End_Object),
           when Key_Name => (VSS.JSON.Events.Key_Name, Reader.Key_Name),
           when String_Value =>
             (VSS.JSON.Events.String_Value, Reader.String_Value),
           when Number_Value =>
             (VSS.JSON.Events.Number_Value, Reader.Number_Value),
           when Boolean_Value =>
             (VSS.JSON.Events.Boolean_Value, Reader.Boolean_Value),
           when Null_Value => (Kind => VSS.JSON.Events.Null_Value),
           when others => raise Program_Error);

      Depth : Natural := 0;
   begin
      while not Reader.At_End loop
         Result.Append (To_Event (Reader));

         Depth := Depth +
            (case Reader.Event_Kind is
               when VSS.JSON.Pull_Readers.Start_Array
                  | VSS.JSON.Pull_Readers.Start_Object => 1,
               when VSS.JSON.Pull_Readers.End_Array
                  | VSS.JSON.Pull_Readers.End_Object => -1,
               when others => 0);

         Reader.Read_Next;

         exit when Depth = 0;
      end loop;
   end Read_Any_JSON_Value;

   --------------------
   -- To_Simple_Type --
   --------------------

   function To_Simple_Type (Value : VSS.Strings.Virtual_String)
     return Definitions.Simple_Types
   is
      use type VSS.Strings.Virtual_String;
   begin
      if Value = "array" then
         return Definitions.An_Array;
      elsif Value = "boolean" then
         return Definitions.A_Boolean;
      elsif Value = "integer" then
         return Definitions.An_Integer;
      elsif Value = "null" then
         return Definitions.A_Null;
      elsif Value = "number" then
         return Definitions.A_Number;
      elsif Value = "object" then
         return Definitions.An_Object;
      elsif Value = "string" then
         return Definitions.A_String;
      else
         raise Program_Error;
      end if;
   end To_Simple_Type;

end JSON_Schema.Readers;
