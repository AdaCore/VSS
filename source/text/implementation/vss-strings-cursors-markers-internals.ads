------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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
