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

with Ada.Containers.Vectors;

with VSS.Markdown.Documents;
with VSS.Strings;
with VSS.Implementation.Markdown;

package VSS.Markdown.Parsers is
   pragma Preelaborate;

   type Markdown_Parser is tagged limited private;
   --  Markdown parser representation

   procedure Parse_Line
     (Self : in out Markdown_Parser'Class;
      Line : VSS.Strings.Virtual_String);
   --  Parse next line of Markdown text and update internal state of the parser

   function Document
     (Self : in out Markdown_Parser) return VSS.Markdown.Documents.Document;
   --  Return parsed document. After this call the Parse_Line has no effect.

private

   use VSS.Implementation.Markdown;

   package Block_Vectors is new Ada.Containers.Vectors
     (Positive, Abstract_Block_Access);

   type Abstract_Container_Block_Access is
     access all Abstract_Container_Block'Class;

   package Container_Vectors is new Ada.Containers.Vectors
     (Positive, Abstract_Container_Block_Access);

   package Block_Detector_Vectors is new Ada.Containers.Vectors
     (Positive, Block_Detector);

   type Parser_State is (Initial, Started, Completed);

   type Markdown_Parser is tagged limited record
      State    : Parser_State := Initial;
      Document : VSS.Markdown.Documents.Document;
      --  Resulting markdown document
      Open      : Container_Vectors.Vector;
      --  Current open container blocks, e.g. blockquote
      Open_Leaf : Abstract_Block_Access;
      --  Current open non-container block (if any), e.g. paragraph
      Block_Detectors  : Block_Detector_Vectors.Vector;
      --  Known block detectors
   end record;

   procedure Register_Block
     (Self     : in out Markdown_Parser'Class;
      Detector : Block_Detector);
   --  Let the parser know a new block kind

end VSS.Markdown.Parsers;
