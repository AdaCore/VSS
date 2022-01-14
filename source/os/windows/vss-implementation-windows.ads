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
--  Low level binding to Windows API.

with Interfaces.C;
pragma Warnings (Off, """System.Win32"" is an internal GNAT unit");
with System.Win32;

package VSS.Implementation.Windows is

   type HANDLE is new System.Win32.HANDLE;

   type BOOL is new System.Win32.BOOL;

   FALSE : constant := System.Win32.FALSE;

   type DWORD is new System.Win32.DWORD;

   type LPCWSTR is access constant Interfaces.C.char16_t;
   type LPWSTR is access all Interfaces.C.char16_t;

   MAX_PATH : constant := 260;

   TOKEN_QUERY : constant := 16#0008#;

   ERROR_INSUFFICIENT_BUFFER : constant := 122;
   ERROR_ENVVAR_NOT_FOUND    : constant := 203;

end VSS.Implementation.Windows;
