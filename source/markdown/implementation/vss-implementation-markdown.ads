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
--  Internal markdown types and methods

with Ada.Containers.Vectors;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with System.Atomic_Counters;

with VSS.Strings;
with VSS.Strings.Character_Iterators;

package VSS.Implementation.Markdown is
   pragma Preelaborate;

   type Abstract_Block is tagged;
   --  A root type for any internal block representation

   type Abstract_Block_Access is access all Abstract_Block'Class;

   type Abstract_Block is abstract tagged limited record
      Counter : System.Atomic_Counters.Atomic_Counter;
   end record;

   function Assigned (Value : access Abstract_Block'Class) return Boolean is
     (Value /= null);
   --  If Value is not null

   function Is_Container (Self : Abstract_Block) return Boolean is (False);
   --  If Self is a container block

   type Input_Line is tagged record
      Text     : VSS.Strings.Virtual_String;
      --  One line of the markdown document without end of line characters
      Expanded : VSS.Strings.Virtual_String;
      --  Text with all tabulatoin characters expanded to spaces
   end record;
   --  One line of the markdown including original and tab expanded values

   function Unexpanded_Tail
     (Self : Input_Line;
      From : VSS.Strings.Character_Iterators.Character_Iterator)
        return VSS.Strings.Virtual_String;
   --  Get From as a position in Self.Expanded and return a slice of Self.Text,
   --  that corresponds to Self.Expanded.Tail (From)

   type Input_Line_Access is access constant Input_Line;

   type Input_Position is record
      Line  : not null Input_Line_Access;
      First : VSS.Strings.Character_Iterators.Character_Iterator;
      --  The position to read from Line.Expanded string
   end record;

   not overriding function Create
     (Input : not null access Input_Position) return Abstract_Block
        is abstract;
   --  Create a new block for given input line. Input should match a
   --  corresponding detector. The Input.First is shifetd to the next posiotion

   subtype Can_Interrupt_Paragraph is Boolean;
   --  if a line can interrupt a paragraph

   not overriding procedure Append_Line
     (Self  : in out Abstract_Block;
      Input : Input_Position;
      CIP   : Can_Interrupt_Paragraph;
      Ok    : in out Boolean) is null;
   --  Append an input line to the block. CIP = True if another block is
   --  detected at the given position and it can interrupt a paragraph.
   --  Return Ok if input was appended to the block.

   package Block_Vectors is new Ada.Containers.Vectors
     (Positive, Abstract_Block_Access);

   type Abstract_Container_Block is abstract new Abstract_Block with record
      Children : Block_Vectors.Vector;
   end record;
   --  A root type for block containing other blocks as children

   overriding function Is_Container (Self : Abstract_Container_Block)
     return Boolean is (True);

   not overriding procedure Consume_Continuation_Markers
     (Self  : in out Abstract_Container_Block;
      Line  : in out Input_Position;
      Match : out Boolean) is abstract;
   --  Set Match to True if Line has continuation markers for the block. If so
   --  shift Line.First to skip the marker.

   type Block_Detector is access procedure
     (Line : Input_Position;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);
   --  The detector checks if given input line starts some markdown block. If
   --  so it returns Tag of the corresponding block type and CIP if the block
   --  can interrupt a paragraph. The markdown parser then construct an object
   --  of that type with Create method.

   procedure Free is new Ada.Unchecked_Deallocation
     (Abstract_Block'Class, Abstract_Block_Access);

end VSS.Implementation.Markdown;
