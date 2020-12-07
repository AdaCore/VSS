------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--  This package is for internal use only.

with VSS.Implementation.Strings;

package VSS.Strings.Internals is

   pragma Preelaborate;

   type String_Data_Constant_Access is
     access constant VSS.Implementation.Strings.String_Data;
   --  This type intended to be used for "hack" code only to use internal
   --  low level string processing API to improve performance of critical
   --  part of the code. It was initially defined to avoid performance penalty
   --  in multitasking applications due to management of accessibility level
   --  value.

   function To_Virtual_String
     (Item : in out VSS.Implementation.Strings.String_Data)
      return VSS.Strings.Virtual_String;
   --  Convert string data into virtual string. Data is references.

   function Data_Access_Constant
     (Self : VSS.Strings.Virtual_String'Class)
      return not null VSS.Strings.Internals.String_Data_Constant_Access;
   --  Return access to string data member of the Virtual_String.

end VSS.Strings.Internals;
