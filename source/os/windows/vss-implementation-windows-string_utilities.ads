------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--  Low level binding to Windows API. String utilities.

with VSS.Strings;

package VSS.Implementation.Windows.String_Utilities is

   type char16_array_access is access all Interfaces.C.char16_array;
   --  String in Windows W native format, allocated with Ada allocator.

   function From_Native_String
     (Item : Interfaces.C.char16_array) return VSS.Strings.Virtual_String;
   --  Convert string from W format of WinAPI into Virtual_String.

   function To_New_Native_String
     (Item : VSS.Strings.Virtual_String) return char16_array_access;
   --  Convert Virtual_String into native representation. Allocated object
   --  may be larger then required to store data, nul terminator is added
   --  at the end of the actual data. If given Virtual_String is 'null'
   --  then function return null.
   --
   --  Memory is allocated by Ada allocator, and must be deallocated with Free
   --  below. Ownership of the string can't be passed to Windows C API.

   function New_Native_String_Buffer
     (Size : Interfaces.C.size_t) return char16_array_access;
   --  Allocates buffer of given size. First index of the allocated buffer
   --  is zero. Additional element is always added for nul terminator.

   procedure Free (Item : in out char16_array_access);
   --  Deallocate memory allocated by New_Native_String.

end VSS.Implementation.Windows.String_Utilities;
