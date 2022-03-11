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

package body JSON_Schema.Readers is

   function To_Simple_Type (Value : VSS.Strings.Virtual_String)
     return Definitions.Simple_Types;
   --  Cast string to Simple_Types

   procedure Read
     (Reader : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value  : out Schema_Access;
      Other  : out Schema_Map;
      Prefix : VSS.Strings.Virtual_String);
   --  Parsea single object from JSON as a schema into the Value and include
   --  all sub-schemas into Other map. Use Prefix for subschema names.

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value  : out Schema_Access;
      Other  : out Schema_Map;
      Prefix : VSS.Strings.Virtual_String)
   is
      use type VSS.Strings.Virtual_String;
      Key    : VSS.Strings.Virtual_String;
      Nested : Schema_Access;
   begin
      pragma Assert (Reader.Is_Start_Object);
      Value := new JSON_Schema.Schema;
      Reader.Read_Next;

      while not Reader.At_End and then not Reader.Is_End_Object loop
         pragma Assert (Reader.Is_Key_Name);
         Key := Reader.Key_Name;
         Reader.Read_Next;

         if Key = "id" then
            pragma Assert (Reader.Is_String_Value);
            Value.Id := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "$schema" then
            pragma Assert (Reader.Is_String_Value);
            Value.URI := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "title" then
            pragma Assert (Reader.Is_String_Value);
            Value.Title := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "description" then
            pragma Assert (Reader.Is_String_Value);
            Value.Description := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "multipleOf" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Multiple_Of := Reader.Number_Value;
            Reader.Read_Next;
         elsif Key = "maximum" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Maximum := Reader.Number_Value;
            Reader.Read_Next;
         elsif Key = "exclusiveMaximum" then
            pragma Assert (Reader.Is_Boolean_Value or Reader.Is_Start_Array);

            if Reader.Is_Boolean_Value then
               Value.Exclusive_Maximum := Reader.Boolean_Value;
               Reader.Read_Next;
            else
               Reader.Skip_Current_Array;  --  ???
            end if;
         elsif Key = "minimum" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Minimum := Reader.Number_Value;
            Reader.Read_Next;
         elsif Key = "exclusiveMinimum" then
            pragma Assert (Reader.Is_Boolean_Value or Reader.Is_Start_Array);

            if Reader.Is_Boolean_Value then
               Value.Exclusive_Minimum := Reader.Boolean_Value;
               Reader.Read_Next;
            else
               Reader.Skip_Current_Array;  --  ???
            end if;
         elsif Key = "maxLength" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Max_Length := Definitions.Positive_Integer
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "minLength" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Min_Length := Definitions.Positive_Integer_Default_0
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "pattern" then
            pragma Assert (Reader.Is_String_Value);
            Value.Pattern := Reader.String_Value;
            Reader.Read_Next;
         elsif Key = "additionalItems" then
            pragma Assert (Reader.Is_Boolean_Value or Reader.Is_Start_Object);

            if Reader.Is_Boolean_Value then
               Value.Additional_Items := (True, Reader.Boolean_Value);
            else
               Read (Reader, Nested, Other, Prefix);
               Value.Additional_Items := (False, Schema => Nested);
            end if;
            Reader.Read_Next;
         elsif Key = "items" then
            pragma Assert (Reader.Is_Start_Object or Reader.Is_Start_Array);

            if Reader.Is_Start_Array then
               Reader.Read_Next;

               while not Reader.At_End and then not Reader.Is_End_Array loop
                  declare
                     Item : Schema_Access;
                  begin
                     Read (Reader, Item, Other, Prefix);
                     Value.Items.Append (Item);
                  end;
               end loop;

               pragma Assert (Reader.Is_End_Array);
               Reader.Read_Next;
            else
               declare
                  Item : Schema_Access;
               begin
                  Read (Reader, Item, Other, Prefix);
                  Value.Items.Append (Item);
               end;
            end if;

         elsif Key = "maxItems" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Max_Items := Definitions.Positive_Integer
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "minItems" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Min_Items := Definitions.Positive_Integer_Default_0
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "uniqueItems" then
            pragma Assert (Reader.Is_Boolean_Value);
            Value.Unique_Items := Reader.Boolean_Value;
            Reader.Read_Next;
         elsif Key = "maxProperties" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Max_Properties := Definitions.Positive_Integer
              (Reader.Number_Value.Integer_Value);
            Reader.Read_Next;
         elsif Key = "minProperties" then
            pragma Assert (Reader.Is_Number_Value);
            Value.Min_Properties := Definitions.Positive_Integer_Default_0
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
            pragma Assert (Reader.Is_Boolean_Value or Reader.Is_Start_Object);

            if Reader.Is_Boolean_Value then
               Value.Additional_Properties := (True, Reader.Boolean_Value);
               Reader.Read_Next;
            else
               Read (Reader, Nested, Other, Prefix);
               Value.Additional_Properties := (False, Schema => Nested);
            end if;

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
                  Read (Reader, Nested, Other, Prefix);
                  Item.Schema := Nested;

                  if Key = "properties" then
                     Value.Properties.Append (Item);
                  else
                     Value.Pattern_Properties.Append (Item);
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
               Read (Reader, Nested, Other, Prefix);
               Value.Dependencies := (False, Schema => Nested);
            end if;

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

         elsif Key = "allOf" or else Key = "anyOf" or else Key = "oneOf" then
            pragma Assert (Reader.Is_Start_Array);
            Reader.Read_Next;

            while not Reader.At_End and then not Reader.Is_End_Array loop
               declare
                  Item : Schema_Access;
               begin
                  Read (Reader, Item, Other, Prefix);

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

            Read (Reader, Value.Negate, Other, Prefix);

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
                  Read (Reader, Item, Other, Name);
                  Other.Insert (Name, Item);
               end;
            end loop;

            pragma Assert (Reader.Is_End_Object);
            Reader.Read_Next;
         end if;

      end loop;

      pragma Assert (Reader.Is_End_Object);
      Reader.Read_Next;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Schema : out Schema_Access;
      Other  : out Schema_Map) is
   begin
      Read (Reader, Schema, Other, "#");
   end Read;

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
