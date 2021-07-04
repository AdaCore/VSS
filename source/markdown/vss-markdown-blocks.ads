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
--  Blocks of markdown. Blocks of some kinds could contain nested blocks.

private with Ada.Finalization;

private with VSS.Implementation.Markdown;

--  limited with VSS.Markdown.Blocks.Paragraphs;

package VSS.Markdown.Blocks is
   pragma Preelaborate;

   type Block is tagged private;
   --  Block element of a markdown document

   function Is_Paragraph (Self : Block) return Boolean;
   --  Check if given block is a paragraph

--   function To_Paragraph (Self : Block)
--     return VSS.Markdown.Blocks.Paragraphs.Paragraph
--        with Pre => Self.Is_Paragraph;
   --  Convert the block to a Paragraph

private

   type Block is new Ada.Finalization.Controlled with record
      Data : VSS.Implementation.Markdown.Abstract_Block_Access;
   end record;

   overriding procedure Adjust (Self : in out Block);
   overriding procedure Finalize (Self : in out Block);

end VSS.Markdown.Blocks;
