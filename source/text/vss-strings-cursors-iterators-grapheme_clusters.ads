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

package VSS.Strings.Cursors.Iterators.Grapheme_Clusters is

   pragma Preelaborate;

   type Grapheme_Cluster_Iterator is
     new Abstract_Segment_Iterator with private;

private

   type Grapheme_Cluster_Iterator is new Abstract_Segment_Iterator with record
      null;
   end record;

   overriding procedure Invalidate (Self : in out Grapheme_Cluster_Iterator);

   overriding procedure String_Modified
     (Self     : in out Grapheme_Cluster_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

   overriding function Forward
     (Self : in out Grapheme_Cluster_Iterator) return Boolean;

   overriding function Has_Element
     (Self : Grapheme_Cluster_Iterator) return Boolean;

   procedure Initialize
     (Self            : in out Grapheme_Cluster_Iterator'Class;
      String          : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor);
   --  Initialize iterator and lookup for grapheme boundaries around the given
   --  position.

end VSS.Strings.Cursors.Iterators.Grapheme_Clusters;
