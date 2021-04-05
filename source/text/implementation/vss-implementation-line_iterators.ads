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

with VSS.Implementation.Strings;
with VSS.Strings;

package VSS.Implementation.Line_Iterators is

   pragma Preelaborate;

   function Forward
     (Data                : VSS.Implementation.Strings.String_Data;
      Terminators         : VSS.Strings.Line_Terminator_Set;
      Initial_Position    : VSS.Implementation.Strings.Cursor;
      First_Position      : out VSS.Implementation.Strings.Cursor;
      Last_Position       : out VSS.Implementation.Strings.Cursor;
      Terminator_Position : out VSS.Implementation.Strings.Cursor)
      return Boolean;
   --  Lookup next line terminator sequence. Initial_Position is cursor at
   --  the last character of previous line. First_Position and Last_Position
   --  are set to location of the first and last characters of the found
   --  line. Terminator_Position is location of the starting character of
   --  the line terminator sequence, or invalid cursor when there is no line
   --  terminator seqeunce found (it is case of last line and moving outside
   --  of data).

end VSS.Implementation.Line_Iterators;
