--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.Referrers;
with VSS.Implementation.Strings;

package VSS.Strings.Cursors.Markers.Internals is

   pragma Preelaborate;

   function New_Character_Marker
     (String   : VSS.Implementation.Referrers.Magic_String_Base'Class;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Create new marker for the given string and position.

   function New_Segment_Marker
     (String : VSS.Implementation.Referrers.Magic_String_Base'Class;
      First  : VSS.Implementation.Strings.Cursor;
      Last   : VSS.Implementation.Strings.Cursor)
      return VSS.Strings.Cursors.Markers.Segment_Marker;
   --  Create a new marker for the given string and bounds.

end VSS.Strings.Cursors.Markers.Internals;
