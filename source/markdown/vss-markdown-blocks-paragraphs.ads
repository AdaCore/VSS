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
--  Markdown paragraph block elements

with VSS.Markdown.Annotations;
private with VSS.Implementation.Markdown.Paragraphs;

package VSS.Markdown.Blocks.Paragraphs is
   pragma Preelaborate;

   type Paragraph is tagged private;
   --  Paragraph block contains annotated inline content

   function Text (Self : Paragraph)
     return VSS.Markdown.Annotations.Annotated_Text;
   --  Return nested annotated text

   function To_Block (Self : Paragraph) return VSS.Markdown.Blocks.Block;
   --  Convert to Block type

   function From_Block (Self : VSS.Markdown.Blocks.Block) return Paragraph;
   --  Convert the Block to Paragraph

private

   type Paragraph_Access is access all
     VSS.Implementation.Markdown.Paragraphs.Paragraph;

   type Paragraph is new Ada.Finalization.Controlled with record
      Data : Paragraph_Access;
   end record;

   overriding procedure Adjust (Self : in out Paragraph);
   overriding procedure Finalize (Self : in out Paragraph);

end VSS.Markdown.Blocks.Paragraphs;
