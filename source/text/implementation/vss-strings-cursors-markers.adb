--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Strings.Cursors.Markers is

   ----------------------
   -- Character_Length --
   ----------------------

   overriding function Character_Length (Self : Segment_Marker)
     return VSS.Strings.Character_Count is
   begin
      return Segment_Cursor_Base (Self).Character_Length;
   end Character_Length;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid (Self : Character_Marker) return Boolean is
   begin
      return Character_Cursor_Base (Self).Is_Valid;
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid (Self : Segment_Marker) return Boolean is
   begin
      return Segment_Cursor_Base (Self).Is_Valid;
   end Is_Valid;

   ---------------------
   -- String_Modified --
   ---------------------

   overriding procedure String_Modified
     (Self     : in out Character_Marker;
      Start    : VSS.Implementation.Strings.Cursor;
      Deleted  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      if VSS.Implementation.Strings.Fixup_Delete
           (Self.Position, Start, Deleted)
      then
         VSS.Implementation.Strings.Fixup_Insert
           (Self.Position, Start, Inserted);

      else
         Self.Invalidate;
         Self.Disconnect;
      end if;
   end String_Modified;

   ---------------------
   -- String_Modified --
   ---------------------

   overriding procedure String_Modified
     (Self     : in out Segment_Marker;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      null;
   end String_Modified;

end VSS.Strings.Cursors.Markers;
