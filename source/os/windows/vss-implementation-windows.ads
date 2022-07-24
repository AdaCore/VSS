--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Low level binding to Windows API.

with Interfaces.C;
pragma Warnings (Off, """System.Win32"" is an internal GNAT unit");
with System.Win32;

package VSS.Implementation.Windows is

   type HANDLE is new System.Win32.HANDLE;

   type HWND is new HANDLE;

   type BOOL is new System.Win32.BOOL;

   FALSE : constant := System.Win32.FALSE;

   type BYTE is new System.Win32.BYTE;
   type UINT is new Interfaces.C.unsigned;
   type WORD is new System.Win32.WORD;
   type DWORD is new System.Win32.DWORD;

   type LPBYTE is access all BYTE;
   type LPCWSTR is access constant Interfaces.C.char16_t;
   type LPWSTR is access all Interfaces.C.char16_t;

   type LPWSTR_Pointer is access all LPWSTR;

   MAX_PATH : constant := 260;

   TOKEN_QUERY : constant := 16#0008#;

   ERROR_INSUFFICIENT_BUFFER : constant := 122;
   ERROR_ENVVAR_NOT_FOUND    : constant := 203;

end VSS.Implementation.Windows;
