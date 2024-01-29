--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Unicode;

separate (Test_Grapheme_Cluster_Iterators)
procedure Test_UCD_Emoji_Test is

   procedure Do_Test (Item : VSS.Strings.Virtual_String);
   --  Execute single testcase

   procedure Parse_Test_Case
     (Buffer : Wide_Wide_String;
      String : out VSS.Strings.Virtual_String);
   --  Parse testcase data

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test (Item : VSS.Strings.Virtual_String) is
      use type VSS.Characters.Virtual_Character;
      use type VSS.Strings.Character_Count;

      Iterator : VSS.Strings.Grapheme_Cluster_Iterators
                   .Grapheme_Cluster_Iterator :=
         Item.Before_First_Grapheme_Cluster;

   begin
      --  ??? unqualified keycap sequences is present in the test file. They
      --  don't match expressions. Skip them now.

      if Item.Character_Length = 2
        and then Item.At_First_Character.Element in '*' | '#' | '0' .. '9'
        and then Item.At_Last_Character.Element
          = VSS.Characters.Virtual_Character'Val (16#20E3#)
      then
         return;
      end if;

      Test_Support.Assert (Iterator.Forward);

      Test_Support.Assert (not Iterator.Element.Is_Empty);
      Test_Support.Assert (Iterator.Is_Emoji);

      Test_Support.Assert (not Iterator.Forward);
   end Do_Test;

   ---------------------
   -- Parse_Test_Case --
   ---------------------

   procedure Parse_Test_Case
     (Buffer : Wide_Wide_String;
      String : out VSS.Strings.Virtual_String)
   is
      First : Positive := Buffer'First;
      Last  : Natural;

   begin
      String.Clear;

      loop
         exit when First > Buffer'Last;

         --  Skip spaces

         for J in First .. Buffer'Last loop
            First := J;

            exit when Buffer (J) /= ' ';
         end loop;

         --  Exit on field separator

         exit when Buffer (First) = ';';

         --  Parse code point

         for J in First .. Buffer'Last loop
            Last := J - 1;

            exit when Buffer (J) not in '0' .. '9' | 'A' .. 'F';
         end loop;

         String.Append
           (VSS.Characters.Virtual_Character'Val
              (VSS.Unicode.Code_Point'Wide_Wide_Value
                   ("16#" & Buffer (First .. Last) & '#')));

         First := Last + 1;
      end loop;
   end Parse_Test_Case;

   File_Name   : constant String :=
     VSS.Strings.Conversions.To_UTF_8_String (Emoji_Root) & "/emoji-test.txt";

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

      --  Lookup for start of the comment

      for J in Buffer'First .. Buffer_Last loop
         Last := J - 1;

         exit when Buffer (J) = '#';
      end loop;

      --  Trim whitespaces at the end of the line.

      for J in reverse Buffer'First .. Last loop
         Last := J;

         exit when Buffer (J)
           not in ' ' | Ada.Characters.Wide_Wide_Latin_1.HT;
      end loop;

      if Last >= Buffer'First then
         declare
            String : VSS.Strings.Virtual_String;

         begin
            Parse_Test_Case (Buffer (Buffer'First .. Last), String);
            Do_Test (String);
         end;
      end if;
   end loop;

   Ada.Wide_Wide_Text_IO.Close (File);
end Test_UCD_Emoji_Test;
