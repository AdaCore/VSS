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
--  Annotated text contains a plain text with all markup removed and
--  a list of corresponding annotations. Each annotation has a segment of
--  the plain text and some addititional information if required.

with Ada.Containers.Vectors;

with VSS.Strings;
with VSS.String_Vectors;
with VSS.Strings.Cursors.Markers;

package VSS.Markdown.Annotations is
   pragma Preelaborate;

   type Annotation_Kind is
     (Soft_Line_Break,
      Emphasis,
      Strong,
      Link,
      Code_Span,
      HTML_Open_Tag,
      HTML_Close_Tag,
      HTML_Comment,
      HTML_Processing_Instruction,
      HTML_Declaration,
      HTML_CDATA);
   --  Kind of annotation for parsed inline content

   type HTML_Attribute is record
      Name  : VSS.Strings.Virtual_String;
      Value : VSS.String_Vectors.Virtual_String_Vector;
      --  An empty vector means no value for the attribute
   end record;
   --  A HTML attribute representation

   package HTML_Attribute_Vectors is new Ada.Containers.Vectors
     (Positive, HTML_Attribute);
   --  A vector of HTML attributes

   type Annotation (Kind : Annotation_Kind := Link) is record
      Segment : VSS.Strings.Cursors.Markers.Segment_Marker;
      --  Corresponding segment in the plain text

      case Kind is
         when Soft_Line_Break |
              Emphasis |
              Strong |
              Code_Span =>
            null;

         when Link =>
            Destination : VSS.Strings.Virtual_String;
            Title       : VSS.String_Vectors.Virtual_String_Vector;
            --  Link title could span several lines

         when HTML_Open_Tag =>
            HTML_Open_Tag   : VSS.Strings.Virtual_String;
            HTML_Attributes : HTML_Attribute_Vectors.Vector;

         when HTML_Close_Tag =>
            HTML_Close_Tag : VSS.Strings.Virtual_String;

         when HTML_Comment =>
            HTML_Comment   : VSS.String_Vectors.Virtual_String_Vector;

         when HTML_Processing_Instruction =>
            HTML_Instruction : VSS.String_Vectors.Virtual_String_Vector;

         when HTML_Declaration =>
            HTML_Declaration : VSS.String_Vectors.Virtual_String_Vector;

         when HTML_CDATA =>
            HTML_CDATA : VSS.String_Vectors.Virtual_String_Vector;
      end case;
   end record;
   --  An annotation for particular inline content segment

   package Annotation_Vectors is new
     Ada.Containers.Vectors (Positive, Annotation);

   type Annotated_Text is tagged record
      Plain_Text : VSS.Strings.Virtual_String;
      Annotation : Annotation_Vectors.Vector;
   end record;
   --  Annotated text contains plain text and a list of annotations

end VSS.Markdown.Annotations;
