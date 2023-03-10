--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Application;
with VSS.Characters;
with VSS.Strings;
with VSS.Text_Streams.File_Input;
with VSS.Text_Streams.File_Output;

procedure Test_File_Text_Streams is
   In_Filename  : constant VSS.Strings.Virtual_String :=
     VSS.Application.Arguments.Element (1);
   In_Stream    : VSS.Text_Streams.File_Input.File_Input_Text_Stream;
   In_Buffer    : VSS.Strings.Virtual_String;

   Out_Filename : constant VSS.Strings.Virtual_String :=
     VSS.Application.Arguments.Element (2);
   Out_Stream   : VSS.Text_Streams.File_Output.File_Output_Text_Stream;

   Character    : VSS.Characters.Virtual_Character;
   Success      : Boolean := True;

begin
   In_Stream.Open (In_Filename);

   while not In_Stream.Is_End_Of_Stream loop
      In_Stream.Get (Character, Success);
      pragma Assert (Success);

      In_Buffer.Append (Character);
   end loop;

   In_Stream.Close;

   Out_Stream.Create (Out_Filename);
   Out_Stream.Put (In_Buffer, Success);
   pragma Assert (Success);
   Out_Stream.Close;
end Test_File_Text_Streams;
