--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with VSS.Characters;
with VSS.Unicode;

procedure Generic_UCD_Break_Test_Runner (File_Name : String) is

   procedure Parse_Test_Case
     (Buffer   : Wide_Wide_String;
      String   : out VSS.Strings.Virtual_String;
      Segments : out VSS.String_Vectors.Virtual_String_Vector);

   ---------------------
   -- Parse_Test_Case --
   ---------------------

   procedure Parse_Test_Case
     (Buffer   : Wide_Wide_String;
      String   : out VSS.Strings.Virtual_String;
      Segments : out VSS.String_Vectors.Virtual_String_Vector)
   is
      First   : Positive := Buffer'First;
      Last    : Natural;
      Segment : VSS.Strings.Virtual_String;

   begin
      String.Clear;
      Segments.Clear;

      if Buffer (First) /= '÷' then
         raise Program_Error;
      end if;

      First := First + 1;
      Segment.Clear;

      loop
         exit when First > Buffer'Last;

         --  Skip spaces

         for J in First .. Buffer'Last loop
            First := J;

            exit when Buffer (J) /= ' ';
         end loop;

         --  Parse code point

         for J in First .. Buffer'Last loop
            Last := J - 1;

            exit when Buffer (J) not in '0' .. '9' | 'A' .. 'F';
         end loop;

         String.Append
           (VSS.Characters.Virtual_Character'Val
              (VSS.Unicode.Code_Point'Wide_Wide_Value
                   ("16#" & Buffer (First .. Last) & '#')));
         Segment.Append
           (VSS.Characters.Virtual_Character'Val
              (VSS.Unicode.Code_Point'Wide_Wide_Value
                   ("16#" & Buffer (First .. Last) & '#')));

         First := Last + 1;

         --  Skip spaces

         for J in First .. Buffer'Last loop
            First := J;

            exit when Buffer (J) /= ' ';
         end loop;

         if Buffer (First) = '÷' then
            Segments.Append (Segment);
            Segment.Clear;
            First := First + 1;

         elsif Buffer (First) = '×' then
            First := First + 1;

         else
            raise Program_Error;
         end if;
      end loop;

      if not Segment.Is_Empty then
         raise Program_Error;
      end if;
   end Parse_Test_Case;

   File        : Ada.Wide_Wide_Text_IO.File_Type;
   Buffer      : Wide_Wide_String (1 .. 1024);
   Buffer_Last : Natural;
   Last        : Natural;

begin
   Ada.Wide_Wide_Text_IO.Open
     (File,
      Ada.Wide_Wide_Text_IO.In_File,
      File_Name,
      "wcem=8");

   while not Ada.Wide_Wide_Text_IO.End_Of_File (File) loop
      Ada.Wide_Wide_Text_IO.Get_Line (File, Buffer, Buffer_Last);
      Last := Buffer_Last;

      for J in Buffer'First .. Buffer_Last loop
         Last := J - 1;

         exit when Buffer (J) = '#';
      end loop;

      for J in reverse Buffer'First .. Last loop
         Last := J;

         exit when Buffer (J)
         not in ' ' | Ada.Characters.Wide_Wide_Latin_1.HT;
      end loop;

      if Last >= Buffer'First then
         declare
            String   : VSS.Strings.Virtual_String;
            Segments : VSS.String_Vectors.Virtual_String_Vector;

         begin
            Parse_Test_Case (Buffer (Buffer'First .. Last), String, Segments);
            Do_Test (String, Segments);
         end;
      end if;
   end loop;

   Ada.Wide_Wide_Text_IO.Close (File);
end Generic_UCD_Break_Test_Runner;
