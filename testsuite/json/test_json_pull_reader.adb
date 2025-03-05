--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Interfaces;

with VSS.Command_Line;
with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Pull_Readers.JSON5;
with VSS.JSON.Streams;
with VSS.Strings.Conversions;
with VSS.Strings.Formatters.Booleans;
with VSS.Strings.Formatters.Generic_Enumerations;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with VSS.String_Vectors;
with VSS.Text_Streams.File_Output;
with VSS.Text_Streams.Standards;

with Tests_Text_Streams;

procedure Test_JSON_Pull_Reader is

--   use all type VSS.JSON.Pull_Readers.JSON_Event_Kind;
   use all type VSS.JSON.Pull_Readers.JSON_Reader_Error;

   package Command_Line is

      procedure Initialize;

   end Command_Line;

   package Options is

      Performance      : Boolean := False;
      Incremental      : Boolean := False;
      JSON5            : Boolean := False;
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

      JSON5_Option        : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => <>,
         Long_Name   => "json5",
         Description => "Enable JSON5");

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
         Options.JSON5 := VSS.Command_Line.Is_Specified (JSON5_Option);

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
         VSS.Command_Line.Add_Option (JSON5_Option);
         VSS.Command_Line.Add_Option (Input_File_Option);
         VSS.Command_Line.Add_Option (Output_File_Option);
      end Register_Switches;

   end Command_Line;

   package JSON_Number_Kind_Formatters is
     new VSS.Strings.Formatters.Generic_Enumerations
           (VSS.JSON.JSON_Number_Kind);

   package JSON_Stream_Element_Kind_Formatters is
     new VSS.Strings.Formatters.Generic_Enumerations
           (VSS.JSON.Streams.JSON_Stream_Element_Kind);

   package JSON_Reader_Error_Formatters is
     new VSS.Strings.Formatters.Generic_Enumerations
           (VSS.JSON.Pull_Readers.JSON_Reader_Error);

   Input  : aliased Tests_Text_Streams.Memory_UTF8_Input_Stream;
   Count  : Natural := 0;
   Output : VSS.Text_Streams.File_Output.File_Output_Text_Stream;
   Error  : VSS.Text_Streams.Output_Text_Stream'Class :=
     VSS.Text_Streams.Standards.Standard_Error;

begin
   Command_Line.Initialize;

   Output.Create (Options.Output_File_Name);

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

      function Setup_Reader
        return VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;

      ------------------
      -- Setup_Reader --
      ------------------

      function Setup_Reader
        return VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class is
      begin
         Input.Set_Incremental (Options.Incremental);

         if Options.JSON5 then
            return Result : VSS.JSON.Pull_Readers.JSON5.JSON5_Pull_Reader do
               Result.Set_Stream (Input'Unchecked_Access);
            end return;

         else
            return Result :
              VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader
            do
               Result.Set_Stream (Input'Unchecked_Access);
            end return;
         end if;
      end Setup_Reader;

      Reader  : VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class :=
        Setup_Reader;
      Start   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Success : Boolean := True;

   begin
      while not Reader.At_End loop
         Reader.Read_Next;

         case Reader.Element_Kind is
            when VSS.JSON.Streams.Invalid =>
               if Reader.Error /= Premature_End_Of_Document then
                  Output.Put_Line
                    (VSS.Strings.Templates.To_Virtual_String_Template
                       ("{} {} ""{}""").Format
                         (JSON_Stream_Element_Kind_Formatters.Image
                            (Reader.Element_Kind),
                          JSON_Reader_Error_Formatters.Image (Reader.Error),
                          VSS.Strings.Formatters.Strings.Image
                            (Reader.Error_Message)),
                     Success);

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

            when VSS.JSON.Streams.Key_Name =>
               Count := 0;

               if not Options.Performance then
                  Output.Put_Line
                    (VSS.Strings.Templates.To_Virtual_String_Template
                       ("{} ""{}""").Format
                         (JSON_Stream_Element_Kind_Formatters.Image
                            (Reader.Element_Kind),
                          VSS.Strings.Formatters.Strings.Image
                            (Reader.Key_Name)),
                     Success);
               end if;

            when VSS.JSON.Streams.String_Value =>
               Count := 0;

               if not Options.Performance then
                  Output.Put_Line
                    (VSS.Strings.Templates.To_Virtual_String_Template
                       ("{} ""{}""").Format
                         (JSON_Stream_Element_Kind_Formatters.Image
                            (Reader.Element_Kind),
                          VSS.Strings.Formatters.Strings.Image
                            (Reader.String_Value)),
                     Success);
               end if;

            when VSS.JSON.Streams.Number_Value =>
               Count := 0;

               if not Options.Performance then
                  case Reader.Number_Value.Kind is
                     when VSS.JSON.None =>
                        Output.Put_Line
                          (VSS.Strings.Templates.To_Virtual_String_Template
                             ("{} {} {}").Format
                               (JSON_Stream_Element_Kind_Formatters.Image
                                    (Reader.Element_Kind),
                                JSON_Number_Kind_Formatters.Image
                                  (Reader.Number_Value.Kind),
                                VSS.Strings.Formatters.Strings.Image
                                  (Reader.Number_Value.String_Value)),
                           Success);

                     when VSS.JSON.JSON_Integer =>
                        Output.Put_Line
                          (VSS.Strings.Templates.To_Virtual_String_Template
                             ("{} {} {} {}").Format
                               (JSON_Stream_Element_Kind_Formatters.Image
                                    (Reader.Element_Kind),
                                JSON_Number_Kind_Formatters.Image
                                  (Reader.Number_Value.Kind),
                                VSS.Strings.Formatters.Strings.Image
                                  (Reader.Number_Value.String_Value),
                                VSS.Strings.Formatters.Strings.Image
                                  (VSS.Strings.To_Virtual_String
                                     (Interfaces.Integer_64'Wide_Wide_Image
                                        (Reader.Number_Value.Integer_Value)))),
                           Success);

                     when VSS.JSON.JSON_Float =>
                        Output.Put_Line
                          (VSS.Strings.Templates.To_Virtual_String_Template
                             ("{} {} {} {}").Format
                               (JSON_Stream_Element_Kind_Formatters.Image
                                    (Reader.Element_Kind),
                                JSON_Number_Kind_Formatters.Image
                                  (Reader.Number_Value.Kind),
                                VSS.Strings.Formatters.Strings.Image
                                  (Reader.Number_Value.String_Value),
                                VSS.Strings.Formatters.Strings.Image
                                  (VSS.Strings.To_Virtual_String
                                     (Interfaces.IEEE_Float_64'Wide_Wide_Image
                                        (Reader.Number_Value.Float_Value)))),
                           Success);

                     when VSS.JSON.Out_Of_Range =>
                        Output.Put_Line
                          (VSS.Strings.Templates.To_Virtual_String_Template
                             ("{} {} {}").Format
                               (JSON_Stream_Element_Kind_Formatters.Image
                                    (Reader.Element_Kind),
                                JSON_Number_Kind_Formatters.Image
                                  (Reader.Number_Value.Kind),
                                VSS.Strings.Formatters.Strings.Image
                                  (Reader.Number_Value.String_Value)),
                           Success);
                  end case;
               end if;

            when VSS.JSON.Streams.Boolean_Value =>
               Count := 0;

               if not Options.Performance then
                  Output.Put_Line
                    (VSS.Strings.Templates.To_Virtual_String_Template
                       ("{} {}").Format
                         (JSON_Stream_Element_Kind_Formatters.Image
                            (Reader.Element_Kind),
                          VSS.Strings.Formatters.Booleans.Image
                            (Reader.Boolean_Value)),
                     Success);
               end if;

            when others =>
               Count := 0;

               if not Options.Performance then
                  Output.Put_Line
                    (VSS.Strings.Templates.To_Virtual_String_Template
                       ("{}").Format
                         (JSON_Stream_Element_Kind_Formatters.Image
                              (Reader.Element_Kind)),
                     Success);
               end if;
         end case;
      end loop;

      if Options.Performance then
         Error.Put_Line
           (VSS.Strings.To_Virtual_String
              (Duration'Wide_Wide_Image (Ada.Calendar.Clock - Start)),
            Success);
      end if;
   end;

   Output.Close;
end Test_JSON_Pull_Reader;
