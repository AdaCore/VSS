--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Low level binding to Windows API (ADVAPI32.DLL).

package VSS.Implementation.Windows.Advapi32 is

   pragma Linker_Options ("-ladvapi32");

   function OpenProcessToken
     (ProcessHandle : HANDLE;
      DesiredAccess : DWORD;
      TokenHandle   : out HANDLE)
      return BOOL
     with Import, Convention => Stdcall, External_Name => "OpenProcessToken";

end VSS.Implementation.Windows.Advapi32;
