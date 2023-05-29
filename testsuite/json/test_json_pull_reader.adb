--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;

with VSS.Command_Line;
with VSS.JSON.Pull_Readers.Simple;
with VSS.Strings.Conversions;
with VSS.String_Vectors;

with Tests_Text_Streams;

procedure Test_JSON_Pull_Reader is

   use all type VSS.JSON.Pull_Readers.JSON_Event_Kind;
   use all type VSS.JSON.Pull_Readers.JSON_Reader_Error;

   package Command_Line is

      procedure Initialize;

   end Command_Line;

   package Options is

      Performance      : Boolean := False;
      Incremental      : Boolean := False;
      Input_File_Name  : VSS.Strings.Virtual_String;
      Output_File_Name : VSS.Strings.Virtual_String;

   end Options;

   -----------------
   -- Command_Lne --
   -----------------

   package body Command_Line is

      Incremental_Option  : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => <>,
         Long_Name   => "incremental",
         Description => "Activate incremental parsing mode");

      Performance_Option  : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => <>,
         Long_Name   => "performance",
         Description => "Report performance statistic");

      Input_File_Option   : constant VSS.Command_Line.Positional_Option :=
        (Name        => "input",
         Description => "Name of the input file");

      Output_File_Option  : constant VSS.Command_Line.Positional_Option :=
        (Name        => "output",
         Description => "Name of the input file");

      procedure Register_Switches;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
         Positionals : VSS.String_Vectors.Virtual_String_Vector;

      begin
         Register_Switches;

         VSS.Command_Line.Process;

         Options.Incremental :=
           VSS.Command_Line.Is_Specified (Incremental_Option);
         Options.Performance :=
           VSS.Command_Line.Is_Specified (Performance_Option);

         Positionals := VSS.Command_Line.Positional_Arguments;

         if Positionals.Length = 0 then
            VSS.Command_Line.Report_Error ("Input file not specified");
         end if;

         if Positionals.Length = 1 then
            VSS.Command_Line.Report_Error ("Output file not specified");
         end if;

         Options.Input_File_Name  :=
           VSS.Command_Line.Value (Input_File_Option);
         Options.Output_File_Name :=
           VSS.Command_Line.Value (Output_File_Option);
      end Initialize;

      -----------------------
      -- Register_Switches --
      -----------------------

      procedure Register_Switches is
      begin
         VSS.Command_Line.Add_Option (Incremental_Option);
         VSS.Command_Line.Add_Option (Performance_Option);
         VSS.Command_Line.Add_Option (Input_File_Option);
         VSS.Command_Line.Add_Option (Output_File_Option);
      end Register_Switches;

   end Command_Line;

   Input       : aliased Tests_Text_Streams.Memory_UTF8_Input_Stream;
   Reader      : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   Count       : Natural := 0;
   Log_File    : Ada.Text_IO.File_Type;

begin
   Command_Line.Initialize;

   Ada.Text_IO.Create
     (Log_File,
      Ada.Text_IO.Out_File,
      VSS.Strings.Conversions.To_UTF_8_String (Options.Output_File_Name),
      "text_translation=no");

   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Aux  : Ada.Streams.Stream_Element_Array (1 .. 1_024);
      Last : Ada.Streams.Stream_Element_Offset;

   begin
      Ada.Streams.Stream_IO.Open
        (File,
         Ada.Streams.Stream_IO.In_File,
         VSS.Strings.Conversions.To_UTF_8_String
           (Options.Input_File_Name),
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
      Input.Set_Incremental (Options.Incremental);
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

               elsif not Options.Incremental then
                  raise Program_Error;

               else
                  Count := Count + 1;

                  if Count > 1_000 then
                     raise Program_Error;
                  end if;
               end if;

            when Key_Name =>
               Count := 0;

               if not Options.Performance then
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

               if not Options.Performance then
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

               if not Options.Performance then
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

               if not Options.Performance then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & " "
                     & Boolean'Image (Reader.Boolean_Value));
               end if;

            when others =>
               Count := 0;

               if not Options.Performance then
                  Ada.Text_IO.Put_Line
                    (Log_File,
                     VSS.JSON.Pull_Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind));
               end if;
         end case;
      end loop;

      if Options.Performance then
         Ada.Text_IO.Put_Line (Duration'Image (Ada.Calendar.Clock - Start));
      end if;
   end;

   Ada.Text_IO.Close (Log_File);
end Test_JSON_Pull_Reader;
