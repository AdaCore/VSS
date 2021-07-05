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
--  Internal representation of a markdown paragraph

with VSS.String_Vectors;

with VSS.Markdown.Annotations;

package VSS.Implementation.Markdown.Paragraphs is
   pragma Preelaborate;

   type Paragraph is new Abstract_Block with private;
   --  Paragraph block contains annotated inline content

   function Text (Self : Paragraph)
     return VSS.Markdown.Annotations.Annotated_Text;
   --  Return nested annotated text

   procedure Detector
     (Line : Input_Position;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);
   --  The detector procedure to find start of a paragraph

private

   type Paragraph is new Abstract_Block with record
      Lines : VSS.String_Vectors.Virtual_String_Vector;
   end record;

   overriding function Create
     (Input : not null access Input_Position) return Paragraph;

   overriding procedure Append_Line
     (Self  : in out Paragraph;
      Input : Input_Position;
      CIP   : Can_Interrupt_Paragraph;
      Ok    : in out Boolean);

end VSS.Implementation.Markdown.Paragraphs;
