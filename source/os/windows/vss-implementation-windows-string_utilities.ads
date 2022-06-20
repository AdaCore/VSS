--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   function From_Native_String
     (Item : LPWSTR) return VSS.Strings.Virtual_String;
   --  Convert string from W format of WinAPI into Virtual_String.

end VSS.Implementation.Windows.String_Utilities;
