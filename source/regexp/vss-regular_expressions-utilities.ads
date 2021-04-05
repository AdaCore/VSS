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

package VSS.Regular_Expressions.Utilities is

   pragma Preelaborate;

   function Escape
     (Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Escapes all characters of the Item that they don't have any special
   --  meaning when used as regular expression pattern string and returns
   --  escaped string.

   function Wildcard_To_Regular_Expression
     (Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Returns a regular expression representation of the glob pattern Item.

end VSS.Regular_Expressions.Utilities;
