--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Strings.Cursors.Markers.Internals is

   --------------------------
   -- New_Character_Marker --
   --------------------------

   function New_Character_Marker
     (String   : VSS.Implementation.Referrers.Magic_String_Base'Class;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return Result : VSS.Strings.Cursors.Markers.Character_Marker do
         if not VSS.Implementation.Strings.Is_Invalid (Position) then
            Result.Position := Position;
            Result.Connect (String'Unrestricted_Access);
         end if;
      end return;
   end New_Character_Marker;

   ------------------------
   -- New_Segment_Marker --
   ------------------------

   function New_Segment_Marker
     (String : VSS.Implementation.Referrers.Magic_String_Base'Class;
      First  : VSS.Implementation.Strings.Cursor;
      Last   : VSS.Implementation.Strings.Cursor)
      return VSS.Strings.Cursors.Markers.Segment_Marker is
   begin
      return Result : VSS.Strings.Cursors.Markers.Segment_Marker do
         Result.First_Position := First;
         Result.Last_Position := Last;
         Result.Connect (String'Unrestricted_Access);
      end return;
   end New_Segment_Marker;

end VSS.Strings.Cursors.Markers.Internals;
