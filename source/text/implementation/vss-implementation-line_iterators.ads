--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Strings;
with VSS.Strings;

package VSS.Implementation.Line_Iterators is

   pragma Preelaborate;

   function Forward
     (Text                : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Terminators         : VSS.Strings.Line_Terminator_Set;
      Initial_Position    : VSS.Implementation.Strings.Cursor;
      First_Position      : aliased out VSS.Implementation.Strings.Cursor;
      Last_Position       : aliased out VSS.Implementation.Strings.Cursor;
      Terminator_Position : out VSS.Implementation.Strings.Cursor)
      return Boolean;
   --  Lookup next line terminator sequence. Initial_Position is cursor at
   --  the last character of previous line. First_Position and Last_Position
   --  are set to location of the first and last characters of the found
   --  line. Terminator_Position is location of the starting character of
   --  the line terminator sequence, or invalid cursor when there is no line
   --  terminator sequence found (it is case of last line and moving outside
   --  of data).

   function Backward
     (Text                : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Terminators         : VSS.Strings.Line_Terminator_Set;
      Initial_Position    : VSS.Implementation.Strings.Cursor;
      First_Position      : aliased out VSS.Implementation.Strings.Cursor;
      Last_Position       : out VSS.Implementation.Strings.Cursor;
      Terminator_Position : out VSS.Implementation.Strings.Cursor)
      return Boolean;
   --  Lookup previous line and line terminator sequence. Initial_Position
   --  is a cursor at the first character of the next line.

end VSS.Implementation.Line_Iterators;
