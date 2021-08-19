------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with VSS.Characters;
with VSS.String_Vectors;
with VSS.Strings.Grapheme_Cluster_Iterators;
with VSS.Unicode;

with Test_Support;

procedure Test_Grapheme_Cluster_Iterators is

   type Test_Case is record
      String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector;
   end record;

   function Parse_Test_Case (Buffer : Wide_Wide_String) return Test_Case;

   procedure Run_Test_Case (Data : Test_Case);

   ---------------------
   -- Parse_Test_Case --
   ---------------------

   function Parse_Test_Case (Buffer : Wide_Wide_String) return Test_Case is
      First   : Positive := Buffer'First;
      Last    : Natural;
      Segment : VSS.Strings.Virtual_String;

   begin
      return Result : Test_Case do
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

            Result.String.Append
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
               Result.Segments.Append (Segment);
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
      end return;
   end Parse_Test_Case;

   -------------------
   -- Run_Test_Case --
   -------------------

   procedure Run_Test_Case (Data : Test_Case) is

      use type VSS.Strings.Virtual_String;

      J : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator :=
        Data.String.First_Grapheme_Cluster;
      S : Positive := 1;

   begin
      --  Test_Support.Assert (not J.Has_Element);

      loop
         --  Test_Support.Assert (J.Has_Element);
         --  Test_Support.Assert (J.Element = Data.Segments (S));
         Test_Support.Assert (Data.String.Slice (J) = Data.Segments (S));
         S := S + 1;

         exit when not J.Forward;
      end loop;

      Test_Support.Assert (S - 1 = Data.Segments.Length);
      --  Test_Support.Assert (not J.Has_Element);
   end Run_Test_Case;

   File        : Ada.Wide_Wide_Text_IO.File_Type;
   Buffer      : Wide_Wide_String (1 .. 1024);
   Buffer_Last : Natural;
   Last        : Natural;

begin
   Ada.Wide_Wide_Text_IO.Open
     (File,
      Ada.Wide_Wide_Text_IO.In_File,
      Ada.Command_Line.Argument (1) & "/auxiliary/GraphemeBreakTest.txt",
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

         exit when Buffer (J) not in ' ' | Ada.Characters.Wide_Wide_Latin_1.HT;
      end loop;

      if Last >= Buffer'First then
         Run_Test_Case (Parse_Test_Case (Buffer (Buffer'First .. Last)));
      end if;
   end loop;

   Ada.Wide_Wide_Text_IO.Close (File);
end Test_Grapheme_Cluster_Iterators;
