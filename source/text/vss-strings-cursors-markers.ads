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

package VSS.Strings.Cursors.Markers is

   pragma Preelaborate;

   type Character_Marker is
     new VSS.Strings.Cursors.Abstract_Character_Cursor with private;

   overriding function Is_Valid (Self : Character_Marker) return Boolean;
   --  GNAT 20210421: This function is invisible overwise.

   type Segment_Marker is
     new VSS.Strings.Cursors.Abstract_Segment_Cursor with private;

private

   type Character_Marker is
     new VSS.Strings.Cursors.Character_Cursor_Base with null record;

   overriding procedure String_Modified
     (Self     : in out Character_Marker;
      Start    : VSS.Implementation.Strings.Cursor;
      Deleted  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

   type Segment_Marker is
     new VSS.Strings.Cursors.Segment_Cursor_Base with null record;

   overriding procedure String_Modified
     (Self     : in out Segment_Marker;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

end VSS.Strings.Cursors.Markers;
