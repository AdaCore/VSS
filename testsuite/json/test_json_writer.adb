--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;

with VSS.JSON.Events;
with VSS.JSON.Push_Writers;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Output;

with Tests_Text_Streams;

procedure Test_JSON_Writer is

   All_Controls : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String
       ((Character'Val (16#00#),
        Character'Val (16#01#),
        Character'Val (16#02#),
        Character'Val (16#03#),
        Character'Val (16#04#),
        Character'Val (16#05#),
        Character'Val (16#06#),
        Character'Val (16#07#),
        Character'Val (16#08#),
        Character'Val (16#09#),
        Character'Val (16#0A#),
        Character'Val (16#0B#),
        Character'Val (16#0C#),
        Character'Val (16#0D#),
        Character'Val (16#0E#),
        Character'Val (16#0F#),
        Character'Val (16#10#),
        Character'Val (16#11#),
        Character'Val (16#12#),
        Character'Val (16#13#),
        Character'Val (16#14#),
        Character'Val (16#15#),
        Character'Val (16#16#),
        Character'Val (16#17#),
        Character'Val (16#18#),
        Character'Val (16#19#),
        Character'Val (16#1A#),
        Character'Val (16#1B#),
        Character'Val (16#1C#),
        Character'Val (16#1D#),
        Character'Val (16#1E#),
        Character'Val (16#1F#)));
   --  All control characters required to be escaped

   Escaped : constant VSS.Strings.Virtual_String := "\""";

   procedure Test_Output_Failure
     (Writer : in out
        VSS.JSON.Push_Writers.JSON_Simple_Push_Writer'Class);

   -------------------------
   -- Test_Output_Failure --
   -------------------------

   procedure Test_Output_Failure
     (Writer : in out
        VSS.JSON.Push_Writers.JSON_Simple_Push_Writer'Class)
   is
      type Test_Event is record
         Length : VSS.Strings.Character_Count;
         Event  : VSS.JSON.Events.JSON_Event;
      end record;

      type Test_Scenario is array (Positive range <>) of Test_Event;

      procedure Run_Test (Scenario : Test_Scenario);

      --------------
      -- Run_Test --
      --------------

      procedure Run_Test (Scenario : Test_Scenario) is
         use type VSS.Strings.Character_Count;

         Limit : VSS.Strings.Character_Count := 0;

      begin
         loop
            declare
               Stream  : aliased Tests_Text_Streams.String_Output_Stream;
               Success : Boolean  := True;
               --  Step    : Positive := 1;
               Length  : VSS.Strings.Character_Count := 0;

            begin
               Stream.Set_Limit (Limit);

               Writer.Set_Stream (Stream'Unchecked_Access);

               Writer.Start_Document (Success);

               if not Success then
                  raise Program_Error;
               end if;

               for J in Scenario'Range loop
                  Length := Length + Scenario (J).Length;

                  case Scenario (J).Event.Kind is
                     when VSS.JSON.Events.Start_Array =>
                        Writer.Start_Array (Success);

                     when VSS.JSON.Events.End_Array =>
                        Writer.End_Array (Success);

                     when VSS.JSON.Events.Start_Object =>
                        Writer.Start_Object (Success);

                     when VSS.JSON.Events.End_Object =>
                        Writer.End_Object (Success);

                     when VSS.JSON.Events.Key_Name =>
                        Writer.Key_Name (Scenario (J).Event.Key, Success);

                     when VSS.JSON.Events.String_Value =>
                        Writer.String_Value
                          (Scenario (J).Event.String_Value, Success);

                     when VSS.JSON.Events.Number_Value =>
                        case Scenario (J).Event.Number_Value.Kind is
                           when VSS.JSON.JSON_Integer =>
                              Writer.Integer_Value
                                (Scenario (J).Event.Number_Value.Integer_Value,
                                 Success);

                           when VSS.JSON.JSON_Float =>
                              Writer.Float_Value
                                (Scenario (J).Event.Number_Value.Float_Value,
                                 Success);

                           when others =>
                              raise Program_Error;
                        end case;

                     when VSS.JSON.Events.Boolean_Value =>
                        Writer.Boolean_Value
                          (Scenario (J).Event.Boolean_Value, Success);

                     when VSS.JSON.Events.Null_Value =>
                        Writer.Null_Value (Success);

                     when others =>
                        raise Program_Error;
                  end case;

                  if Length <= Limit then
                     if not Success then
                        Ada.Text_IO.Put_Line
                          (VSS.Strings.Conversions.To_UTF_8_String
                             (Stream.Buffer));

                        raise Program_Error;
                     end if;

                  else
                     if Success then
                        Ada.Text_IO.Put_Line
                          (VSS.Strings.Conversions.To_UTF_8_String
                             (Stream.Buffer));

                        raise Program_Error;
                     end if;

                     exit;
                  end if;
               end loop;

               Writer.End_Document (Success);

               --  ??? Generated output can be compared here to match expected

               exit when Length < Limit;
            end;

            Limit := Limit + 1;
         end loop;
      end Run_Test;

      --  All kinds of events as elements of arrays, to check failure at array
      --  element delimiter. It tests many cases for primitive types too.
      All_Array_Scenario : constant Test_Scenario :=
        ((1, (Kind => VSS.JSON.Events.Start_Array)),
         (4, (Kind => VSS.JSON.Events.Null_Value)),
         (2, (Kind => VSS.JSON.Events.Start_Array)),
         (1, (Kind => VSS.JSON.Events.End_Array)),
         (2, (Kind => VSS.JSON.Events.Start_Object)),
         (1, (Kind => VSS.JSON.Events.End_Object)),
         (3, (Kind         => VSS.JSON.Events.String_Value,
              String_Value => VSS.Strings.Empty_Virtual_String)),
         (2, (Kind         => VSS.JSON.Events.Number_Value,
              Number_Value => (Kind          => VSS.JSON.JSON_Integer,
                               String_Value  =>
                                 VSS.Strings.Empty_Virtual_String,
                               Integer_Value => 0))),
         (21, (Kind         => VSS.JSON.Events.Number_Value,
               Number_Value => (Kind          => VSS.JSON.JSON_Float,
                                String_Value  =>
                                  VSS.Strings.Empty_Virtual_String,
                                Float_Value   => 0.0))),
         (6, (Kind          => VSS.JSON.Events.Boolean_Value,
              Boolean_Value => False)),
         (5, (Kind          => VSS.JSON.Events.Boolean_Value,
              Boolean_Value => True)),
         (5, (Kind => VSS.JSON.Events.Null_Value)),
         (1, (Kind => VSS.JSON.Events.End_Array)));

      --  Few key-value pairs in the object to check failure at pairs
      --  delimiter.
      Object_Key_Scenario : constant Test_Scenario :=
        ((1, (Kind => VSS.JSON.Events.Start_Object)),
         (7, (Kind => VSS.JSON.Events.Key_Name,
              Key  => "name")),
         (2, (Kind         => VSS.JSON.Events.String_Value,
              String_Value => VSS.Strings.Empty_Virtual_String)),
         (11, (Kind => VSS.JSON.Events.Key_Name,
               Key  => "surname")),
         (2, (Kind         => VSS.JSON.Events.String_Value,
              String_Value => VSS.Strings.Empty_Virtual_String)),
         (1, (Kind => VSS.JSON.Events.End_Object)));

      --  All control characters in the string literal
      All_Controls_Scenario : constant Test_Scenario :=
        (1 =>
           (174, (Kind         => VSS.JSON.Events.String_Value,
                  String_Value => All_Controls)));

      Escaped_Scenario : constant Test_Scenario :=
        (1 =>
           (6, (Kind         => VSS.JSON.Events.String_Value,
                String_Value => Escaped)));

   begin
      --  This test cover simplest cases only, content of the generated JSON
      --  is not checked.

      Run_Test (All_Array_Scenario);
      Run_Test (Object_Key_Scenario);
      Run_Test (All_Controls_Scenario);
      Run_Test (Escaped_Scenario);
   end Test_Output_Failure;

   use type Interfaces.IEEE_Float_64;
   use type Interfaces.Integer_64;

   Stream  :
     aliased VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;
   Writer  : aliased VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
   Success : Boolean := True;

begin
   Writer.Set_Stream (Stream'Unchecked_Access);
   Writer.Start_Document (Success);

   Writer.Start_Object (Success);

   --  Some usual constructs.

   Writer.Key_Name ("name", Success);
   Writer.String_Value ("Some name", Success);
   Writer.Key_Name ("names", Success);
   Writer.Start_Array (Success);
   Writer.String_Value ("Some", Success);
   Writer.String_Value ("name", Success);
   Writer.End_Array (Success);
   Writer.Key_Name ("is", Success);
   Writer.Boolean_Value (False, Success);
   Writer.Key_Name ("no", Success);
   Writer.Boolean_Value (True, Success);
   Writer.Key_Name ("empty", Success);
   Writer.Null_Value (Success);
   Writer.Key_Name ("integer", Success);
   Writer.Integer_Value (15, Success);
   Writer.Key_Name ("float", Success);
   Writer.Float_Value (20.5, Success);

   --  Arrays of different types

   Writer.Key_Name ("booleans", Success);
   Writer.Start_Array (Success);
   Writer.Boolean_Value (False, Success);
   Writer.Boolean_Value (True, Success);
   Writer.End_Array (Success);

   Writer.Key_Name ("nulls", Success);
   Writer.Start_Array (Success);
   Writer.Null_Value (Success);
   Writer.Null_Value (Success);
   Writer.End_Array (Success);

   Writer.Key_Name ("floats", Success);
   Writer.Start_Array (Success);
   Writer.Float_Value (-1.0, Success);
   Writer.Float_Value (1.0, Success);
   Writer.End_Array (Success);

   Writer.Key_Name ("integers", Success);
   Writer.Start_Array (Success);
   Writer.Integer_Value (-1, Success);
   Writer.Integer_Value (1, Success);
   Writer.End_Array (Success);

   Writer.Key_Name ("arrays", Success);
   Writer.Start_Array (Success);
   Writer.Start_Array (Success);
   Writer.End_Array (Success);
   Writer.Start_Array (Success);
   Writer.End_Array (Success);
   Writer.End_Array (Success);

   Writer.Key_Name ("objects", Success);
   Writer.Start_Array (Success);
   Writer.Start_Object (Success);
   Writer.End_Object (Success);
   Writer.Start_Object (Success);
   Writer.End_Object (Success);
   Writer.End_Array (Success);

   --  All control characters inside string value

   Writer.Key_Name ("controls", Success);
   Writer.String_Value (All_Controls, Success);

   --  Backslash and quotation mark inside string value

   Writer.Key_Name ("others", Success);
   Writer.String_Value (Escaped, Success);

   --  Empty string as value (for both null string and string of zero length
   --  cases)

   Writer.Key_Name ("empty_string", Success);
   Writer.String_Value ("", Success);
   Writer.Key_Name ("null_string", Success);
   Writer.String_Value (VSS.Strings.Empty_Virtual_String, Success);

   Writer.End_Object (Success);
   Writer.End_Document (Success);

   declare
      File : Ada.Streams.Stream_IO.File_Type;

   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));

      declare
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Offset;

         Expected : Ada.Streams.Stream_Element_Array
           (1 .. Ada.Streams.Stream_Element_Count
                   (Ada.Streams.Stream_IO.Size (File)));
         Last     : Ada.Streams.Stream_Element_Count;

      begin
         Ada.Streams.Stream_IO.Read (File, Expected, Last);

         if Last /= Expected'Last or Last = 0 then
            raise Program_Error;
         end if;

         for J in Expected'Range loop
            if Expected (J) /= Stream.Buffer.Element (J) then
               Ada.Text_IO.Put
                 (VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String
                    (Stream.Buffer));

               raise Program_Error;
            end if;
         end loop;
      end;
   end;

   declare
      Writer : aliased VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;

   begin
      Test_Output_Failure (Writer);
   end;
end Test_JSON_Writer;
