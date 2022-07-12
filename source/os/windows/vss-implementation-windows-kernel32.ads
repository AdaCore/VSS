--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Low level binding to Windows API (KERNEL32.DLL).

package VSS.Implementation.Windows.Kernel32 is

   pragma Linker_Options ("-lkernel32");

   function CloseHandle (hObject : HANDLE) return BOOL
     with Import, Convention => Stdcall, Link_Name => "CloseHandle";

   function GetCommandLine return LPWSTR
     with Import, Convention => Stdcall, Link_Name => "GetCommandLineW";

   function GetCurrentProcess return HANDLE
     with Import, Convention => Stdcall, Link_Name => "GetCurrentProcess";

   function GetLastError return DWORD
     with Import, Convention => Stdcall, Link_Name => "GetLastError";

   function GetEnvironmentVariable
     (lpName   : LPCWSTR;
      lpBuffer : LPWSTR;
      nSize    : DWORD) return DWORD
     with Import,
          Convention => Stdcall,
          Link_Name  => "GetEnvironmentVariableW";

   function GetTempPath
     (nBufferLength : DWORD;
      lpBuffer      : LPWSTR) return DWORD
      with Import, Convention => Stdcall, Link_Name => "GetTempPathW";

   function GetLongPathName
     (lpszShortPath : LPCWSTR;
      lpszLongPath  : LPWSTR;
      cchBuffer     : DWORD) return DWORD
      with Import, Convention => Stdcall, Link_Name => "GetLongPathNameW";

   function GetModuleFileName
     (hModule    : HANDLE;
      lpFilename : LPWSTR;
      nSize      : DWORD) return DWORD
     with Import, Convention => StdCall, Link_Name => "GetModuleFileNameW";

   procedure LocalFree (hMem : LPWSTR_Pointer)
     with Import, Convention => StdCall, Link_Name => "LocalFree";

end VSS.Implementation.Windows.Kernel32;
