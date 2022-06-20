--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Atomic_Counters;

with VSS.Implementation.Referrers;
with VSS.Implementation.Strings;
with VSS.Strings.Cursors.Markers;

package VSS.Regular_Expressions.Matches is

   pragma Preelaborate;

   type Segment_Marker_Array is array (Positive range <>) of
     VSS.Strings.Cursors.Markers.Segment_Marker;

   type Match (Length : Natural := 0) is
     new VSS.Implementation.Referrers.Referal_Limited_Base with record
      Counter   : System.Atomic_Counters.Atomic_Counter;
      Has_Match : Boolean;
      Markers   : Segment_Marker_Array (1 .. Length);
   end record;

   overriding procedure Invalidate (Self : in out Match);

   overriding procedure String_Modified
     (Self     : in out Match;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

end VSS.Regular_Expressions.Matches;
