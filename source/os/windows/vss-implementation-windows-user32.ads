--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Low level binding to Windows API (USER32.DLL).

package VSS.Implementation.Windows.User32 is

   pragma Linker_Options ("-luser32");

   MB_OK              : constant UINT := 16#0000_0000#;
   MB_ICONERROR       : constant UINT := 16#0000_0010#;
   MB_ICONINFORMATION : constant UINT := 16#0000_0040#;
   MB_SETFOREGROUND   : constant UINT := 16#0001_0000#;
   MB_TOPMOST         : constant UINT := 16#0004_0000#;

   function MessageBox
     (hWnd      : Windows.HWND;
      lpText    : LPCWSTR;
      lpCaption : LPCWSTR;
      uType     : UINT) return Interfaces.C.int
     with Import, Convention => StdCall, Link_Name => "MessageBoxW";

end VSS.Implementation.Windows.User32;
