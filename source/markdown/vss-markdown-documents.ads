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
--  The document is a root node of markdown document representation

private with Ada.Finalization;

with VSS.Markdown.Block_Containers;
with VSS.Markdown.Blocks;
private with VSS.Implementation.Markdown;

package VSS.Markdown.Documents is
   pragma Preelaborate;

   type Document is new VSS.Markdown.Block_Containers.Block_Container
     with private;
   --  Markdown document contains nested block elements

   --  procedure Append
   --  (Self  : in out Document;
   --   Block : VSS.Markdown.Blocks.Block);
   --  Append a new markdown block to the document

private

   type Abstract_Container_Block_Access is access all
     VSS.Implementation.Markdown.Abstract_Container_Block'Class;

   type Document is new Ada.Finalization.Controlled
     and VSS.Markdown.Block_Containers.Block_Container with
   record
      Data : Abstract_Container_Block_Access;
   end record;

   overriding procedure Adjust (Self : in out Document);
   overriding procedure Finalize (Self : in out Document);
   overriding function Is_Empty (Self : Document) return Boolean;
   overriding function Length (Self : Document) return Natural;

   overriding function Element
     (Self  : Document;
      Index : Positive) return VSS.Markdown.Blocks.Block;

end VSS.Markdown.Documents;
