--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Low level binding to Windows API (SHELL32.DLL).

package VSS.Implementation.Windows.Shell32 is

   pragma Linker_Options ("-lshell32");

   function CommandLineToArgv
     (lpCmdLine : LPCWSTR;
      pNumArgs  : out Interfaces.C.int) return LPWSTR_Pointer
     with Import, Convention => Stdcall, Link_Name => "CommandLineToArgvW";

end VSS.Implementation.Windows.Shell32;
