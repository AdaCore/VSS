--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;

with VSS.Strings.Conversions;
with VSS.JSON.Pull_Readers.Simple;

with Tests_Text_Streams;

procedure Test_JSON_Pull_Reader is

   use all type VSS.JSON.Pull_Readers.JSON_Event_Kind;
   use all type VSS.JSON.Pull_Readers.JSON_Reader_Error;

   Input       : aliased Tests_Text_Streams.Memory_UTF8_Input_Stream;
   Reader      : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   Count       : Natural := 0;
   Perfomance  : Boolean := False;
   Incremental : Boolean := False;
   Log_File    : Ada.Text_IO.File_Type;

begin
   if Ada.Command_Line.Argument_Count /= 3 then
      raise Program_Error with "incorrect command line arguments";
   end if;

   if Ada.Command_Line.Argument (1) = "s" then
      Incremental := False;
      Perfomance := False;

   elsif Ada.Command_Line.Argument (1) = "sp" then
      Incremental := False;
      Perfomance := True;

   elsif Ada.Command_Line.Argument (1) = "i" then
      Incremental := True;
      Perfomance := False;

   elsif Ada.Command_Line.Argument (1) = "ip" then
      Incremental := True;
      Perfomance := True;

   else
      raise Program_Error with "incorrect parsing mode";
   end if;

   Ada.Text_IO.Create
     (Log_File,
      Ada.Text_IO.Out_File,
      Ada.Command_Line.Argument (3),
      "text_translation=no");

   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Aux  : Ada.Streams.Stream_Element_Array (1 .. 1_024);
      Last : Ada.Streams.Stream_Element_Offset;

   begin
      Ada.Streams.Stream_IO.Open
        (File,
         Ada.Streams.Stream_IO.In_File,
         Ada.Command_Line.Argument (2),
         "text_translation=no");

      while not Ada.Streams.Stream_IO.End_Of_File (File) loop
         Ada.Streams.Stream_IO.Read (File, Aux, Last);

         for J in Aux'First .. Last loop
            Input.Buffer.Append (Aux (J));
         end loop;
      end loop;

      Ada.Streams.Stream_IO.Close (File);
   end;

   declare
      use type Ada.Calendar.Time;

      Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   begin
      Input.Set_Incremental (Incremental);
      Reader.Set_Stream (Input'Unchecked_Access);

      while not Reader.At_End loop
         Reader.Read_Next;

         case Reader.Event_Kind is
            when Invalid =>
               if Reader.Error /= Premature_End_Of_Document then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)

                     & ' '
                     & VSS.JSON.Pull_Readers.JSON_Reader_Error'Image
                         (Reader.Error)
                     & " """
                     & VSS.Strings.Conversions.To_UTF_8_String
                         (Reader.Error_Message)
                     & '"');

                  if Reader.Error /= Not_Valid then
                     raise Program_Error;
                  end if;

               elsif not Incremental then
                  raise Program_Error;

               else
                  Count := Count + 1;

                  if Count > 1_000 then
                     raise Program_Error;
                  end if;
               end if;

            when Key_Name =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & " """
                     & VSS.Strings.Conversions.To_UTF_8_String
                       (Reader.Key_Name)
                     & '"');
               end if;

            when String_Value =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & " """
                     & VSS.Strings.Conversions.To_UTF_8_String
                       (Reader.String_Value)
                     & '"');
               end if;

            when Number_Value =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & ' '
                     & VSS.JSON.JSON_Number_Kind'Image
                       (Reader.Number_Value.Kind)
                     & ' '
                     & VSS.Strings.Conversions.To_UTF_8_String
                       (Reader.Number_Value.String_Value)
                     & (case Reader.Number_Value.Kind is
                          when VSS.JSON.None => "",
                          when VSS.JSON.Out_Of_Range => "",
                          when VSS.JSON.JSON_Integer =>
                            ' '
                              & Interfaces.Integer_64'Image
                                  (Reader.Number_Value.Integer_Value),
                          when VSS.JSON.JSON_Float   =>
                            ' '
                              & Interfaces.IEEE_Float_64'Image
                                  (Reader.Number_Value.Float_Value)));
               end if;

            when Boolean_Value =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & " "
                     & Boolean'Image (Reader.Boolean_Value));
               end if;

            when others =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind));
               end if;
         end case;
      end loop;

      if Perfomance then
         Ada.Text_IO.Put_Line (Duration'Image (Ada.Calendar.Clock - Start));
      end if;
   end;

   Ada.Text_IO.Close (Log_File);
end Test_JSON_Pull_Reader;
