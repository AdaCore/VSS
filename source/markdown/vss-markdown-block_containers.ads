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
--  Common interface for all blocks with nested blocks

with VSS.Markdown.Blocks;

package VSS.Markdown.Block_Containers is
   pragma Preelaborate;

   type Block_Container is interface
     with
       Constant_Indexing => Element;
   --  Block container is just a vector of markdown block elements

   function Is_Empty (Self : Block_Container) return Boolean is abstract;
   --  Check is the container has no nested blocks

   function Length (Self : Block_Container) return Natural is abstract;
   --  Return number of blocks in the container

   function Element
     (Self  : Block_Container;
      Index : Positive) return VSS.Markdown.Blocks.Block is abstract;
   --  Return a block with given index

end VSS.Markdown.Block_Containers;
