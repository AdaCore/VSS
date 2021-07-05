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
--  This program accepts Markdown on stdin and prints HTML on stdout.
--  See https://github.com/commonmark/commonmark-spec for more details.

with Ada.Wide_Wide_Text_IO;

with VSS.Strings;

with VSS.Markdown.Annotations;
with VSS.Markdown.Block_Containers;
with VSS.Markdown.Blocks;
with VSS.Markdown.Blocks.Paragraphs;
with VSS.Markdown.Documents;
with VSS.Markdown.Parsers;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Cursors.Markers;

with HTML_Writers;

procedure Test_Markdown_Tests is
   pragma Assertion_Policy (Check);

   procedure Print_Block
     (Writer : in out HTML_Writers.Writer;
      Block  : VSS.Markdown.Blocks.Block);

   procedure Print_Blocks
     (Writer : in out HTML_Writers.Writer;
      List   : VSS.Markdown.Block_Containers.Block_Container'Class);

   procedure Print_Annotated_Text
     (Writer : in out HTML_Writers.Writer;
      Text   : VSS.Markdown.Annotations.Annotated_Text);

   procedure Print_Annotated_Text
     (Writer : in out HTML_Writers.Writer;
      Text   : VSS.Markdown.Annotations.Annotated_Text)
   is
      use type VSS.Strings.Character_Count;

      procedure Print
        (From  : in out Positive;
         Next  : in out VSS.Strings.Cursors.Markers.Character_Marker;
         Limit : VSS.Strings.Character_Iterators.Character_Iterator);
      --  From is an index in Text.Annotation to start from
      --  Next is a not printed yet character in Text.Plain_Text
      --  Dont go after Limit position in Text.Plain_Text

      function "<="
        (Segment  : VSS.Strings.Cursors.Markers.Segment_Marker;
         Position : VSS.Strings.Character_Iterators.Character_Iterator)
         return Boolean
      is
        (VSS.Strings.Cursors.Abstract_Cursor'Class (Segment).
           Last_Character_Index <= Position.Character_Index);
      --  Check if Segment ends before Position

      -----------
      -- Print --
      -----------

      procedure Print
        (From  : in out Positive;
         Next  : in out VSS.Strings.Cursors.Markers.Character_Marker;
         Limit : VSS.Strings.Character_Iterators.Character_Iterator) is
      begin
         while From <= Text.Annotation.Last_Index and then
           Text.Annotation (From).Segment <= Limit
         loop
            --  Print annotation here
            null;
            From := From + 1;
         end loop;

         if Next.Character_Index <= Limit.Character_Index then
            Writer.Characters (Text.Plain_Text.Slice (Next, Limit));

            declare
               Iter : VSS.Strings.Character_Iterators.Character_Iterator :=
                 Text.Plain_Text.Character (Limit);
            begin
               if Iter.Forward then
                  Next := Iter.Marker;
               end if;
            end;
         end if;
      end Print;

      From  : Positive := Text.Annotation.First_Index;
      Next  : VSS.Strings.Cursors.Markers.Character_Marker :=
        Text.Plain_Text.First_Character.Marker;
   begin
      Print
        (From  => From,
         Next  => Next,
         Limit => Text.Plain_Text.Last_Character);
   end Print_Annotated_Text;

   -----------------
   -- Print_Block --
   -----------------

   procedure Print_Block
     (Writer : in out HTML_Writers.Writer;
      Block  : VSS.Markdown.Blocks.Block) is
   begin
      if Block.Is_Paragraph then
         Writer.Start_Element ("p");
         Print_Annotated_Text (Writer, Block.To_Paragraph.Text);
         Writer.End_Element ("p");
      else
         raise Program_Error;
      end if;
   end Print_Block;

   procedure Print_Blocks
     (Writer : in out HTML_Writers.Writer;
      List   : VSS.Markdown.Block_Containers.Block_Container'Class) is
   begin
      for J in 1 .. List.Length loop
         declare
            Block : constant VSS.Markdown.Blocks.Block := List.Element (J);
         begin
            Print_Block (Writer, Block);
         end;
      end loop;
   end Print_Blocks;

   Writer : HTML_Writers.Writer;
   Parser : VSS.Markdown.Parsers.Markdown_Parser;
begin
   while not Ada.Wide_Wide_Text_IO.End_Of_File loop
      declare
         Line : constant Wide_Wide_String := Ada.Wide_Wide_Text_IO.Get_Line;
         Text : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String (Line);
      begin
         Parser.Parse_Line (Text);
      end;
   end loop;

   declare
      Document : constant VSS.Markdown.Documents.Document := Parser.Document;
   begin
      --  Writer.Start_Element ("html");
      Print_Blocks (Writer, Document);
      --  Writer.End_Element ("html");
   end;
end Test_Markdown_Tests;
