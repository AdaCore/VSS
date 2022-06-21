--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with VSS.Strings;

with Test_Support;

procedure Test_String_Casing_W3C_I18N is
   use type VSS.Strings.Virtual_String;

   File      : Ada.Wide_Wide_Text_IO.File_Type;
   Line      : Wide_Wide_String (1 .. 1024);
   Last      : Natural;
   Lowercase : Boolean;
   Source    : VSS.Strings.Virtual_String;
   Expected  : VSS.Strings.Virtual_String;

begin
   Ada.Wide_Wide_Text_IO.Open
     (File,
      Ada.Wide_Wide_Text_IO.In_File,
      Ada.Command_Line.Argument (1),
      "wcem=8");

   --  Skip name of the test

   Ada.Wide_Wide_Text_IO.Skip_Line (File);

   --  Read case conversion

   Ada.Wide_Wide_Text_IO.Get_Line (File, Line, Last);

   if Line (Line'First .. Last) = "lowercase" then
      Lowercase := True;

   elsif Line (Line'First .. Last) = "uppercase" then
      Lowercase := False;

   else
      raise Program_Error;
   end if;

   --  Read source string

   Ada.Wide_Wide_Text_IO.Get_Line (File, Line, Last);
   Source := VSS.Strings.To_Virtual_String (Line (Line'First .. Last));
   Test_Support.Assert (not Source.Is_Empty);

   --  Read expected string

   Ada.Wide_Wide_Text_IO.Get_Line (File, Line, Last);
   Expected := VSS.Strings.To_Virtual_String (Line (Line'First .. Last));
   Test_Support.Assert (not Expected.Is_Empty);

   Ada.Wide_Wide_Text_IO.Close (File);

   if Lowercase then
      Test_Support.Assert (Source.To_Lowercase = Expected);

   else
      Test_Support.Assert (Source.To_Uppercase = Expected);
   end if;
end Test_String_Casing_W3C_I18N;
