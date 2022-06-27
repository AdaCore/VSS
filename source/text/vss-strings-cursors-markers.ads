--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package VSS.Strings.Cursors.Markers is

   pragma Preelaborate;

   type Character_Marker is
     new VSS.Strings.Cursors.Abstract_Character_Cursor with private;

   overriding function Is_Valid (Self : Character_Marker) return Boolean;
   --  GNAT 20210421: This function is invisible otherwise.

   type Segment_Marker is
     new VSS.Strings.Cursors.Abstract_Segment_Cursor with private;

   overriding function Is_Valid (Self : Segment_Marker) return Boolean;
   --  GNAT 20210421: This function is invisible otherwise.

   overriding function Character_Length (Self : Segment_Marker)
     return VSS.Strings.Character_Count;
   --  GNAT 20210421: This function is invisible otherwise.

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
