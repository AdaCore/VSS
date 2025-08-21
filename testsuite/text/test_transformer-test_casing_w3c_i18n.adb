--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Wide_Wide_Text_IO;

with VSS.Transformers.Casing;

separate (Test_Transformer)
procedure Test_Casing_W3C_I18N is
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
      VSS.Strings.Conversions.To_UTF_8_String (W3C_I18N_File),
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
      Test_Support.Assert
        (VSS.Transformers.Casing.To_Lowercase.Transform (Source) = Expected);

   else
      Test_Support.Assert
        (VSS.Transformers.Casing.To_Uppercase.Transform (Source) = Expected);
   end if;
end Test_Casing_W3C_I18N;
