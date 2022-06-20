--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;

with GNATCOLL.JSON;

procedure Test_JSON_Reader_GNATCOLL is
   Data     : Ada.Strings.Unbounded.Unbounded_String;
   Document : GNATCOLL.JSON.JSON_Value;

begin
   declare
      File : Ada.Text_IO.File_Type;
      Aux  : Ada.Strings.Unbounded.Unbounded_String;

   begin
      Ada.Text_IO.Open
        (File, Ada.Text_IO.In_File, Ada.Command_Line.Argument (1));

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Strings.Unbounded.Text_IO.Get_Line (File, Aux);
         --  Ada.Strings.Unbounded.Append (Data, ASCII.LF);
         Ada.Strings.Unbounded.Append (Data, Aux);
      end loop;

      Ada.Text_IO.Close (File);
   end;

   declare
      use type Ada.Calendar.Time;

      Start : Ada.Calendar.Time := Ada.Calendar.Clock;

   begin
      Document := GNATCOLL.JSON.Read (Data);
      Ada.Text_IO.Put_Line (Duration'Image (Ada.Calendar.Clock - Start));
   end;
end Test_JSON_Reader_GNATCOLL;
