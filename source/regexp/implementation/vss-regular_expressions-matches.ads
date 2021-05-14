------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with System.Atomic_Counters;
with VSS.Strings.Cursors.Markers;

package VSS.Regular_Expressions.Matches is

   pragma Preelaborate;

   type Segment_Marker_Array is array (Positive range <>) of
     VSS.Strings.Cursors.Markers.Segment_Marker;

   type Match (Length : Natural := 0) is limited record
      Counter   : System.Atomic_Counters.Atomic_Counter;
      Has_Match : Boolean;
      Subject   : VSS.Strings.Virtual_String;
      Markers   : Segment_Marker_Array (1 .. Length);
   end record;

end VSS.Regular_Expressions.Matches;
