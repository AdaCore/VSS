--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C.Pointers;

with VSS.Implementation.Windows.Kernel32;
with VSS.Implementation.Windows.Shell32;
with VSS.Implementation.Windows.String_Utilities;

separate (VSS.Application)
package body Platform is

   type LPWSTR_Array is
     array (Natural range <>) of aliased VSS.Implementation.Windows.LPWSTR;

   package LPWSTR_Pointers is
     new Interfaces.C.Pointers
           (Natural, VSS.Implementation.Windows.LPWSTR, LPWSTR_Array, null);

   ----------------------
   -- Application_File --
   ----------------------

   function Application_File return VSS.Strings.Virtual_String is

      use type VSS.Implementation.Windows.DWORD;

      Size : VSS.Implementation.Windows.DWORD := 512;

   begin
      return Result : VSS.Strings.Virtual_String do
         loop
            declare
               Buffer :
                 Interfaces.C.char16_array (1 .. Interfaces.C.size_t (Size));
               Status : VSS.Implementation.Windows.DWORD;

            begin
               Status :=
                 VSS.Implementation.Windows.Kernel32.GetModuleFileName
                   (0, Buffer (Buffer'First)'Unchecked_Access, Size);

               if Status = 0 then
                  return;
               end if;

               if Status < Size
                 or else VSS.Implementation.Windows.Kernel32.GetLastError
                   /= VSS.Implementation.Windows.ERROR_INSUFFICIENT_BUFFER
               then
                  Result :=
                    VSS.Implementation.Windows.String_Utilities
                      .From_Native_String
                         (Buffer (Buffer'First)'Unchecked_Access);

                  return;
               end if;

               Size := Size * 2;
            end;
         end loop;
      end return;
   end Application_File;

   ---------------
   -- Arguments --
   ---------------

   function Arguments return VSS.String_Vectors.Virtual_String_Vector is
      Argc : Interfaces.C.int;
      Argv : constant VSS.Implementation.Windows.LPWSTR_Pointer :=
        VSS.Implementation.Windows.Shell32.CommandLineToArgv
          (VSS.Implementation.Windows.LPCWSTR
             (VSS.Implementation.Windows.Kernel32.GetCommandLine),
           Argc);
      Args : constant LPWSTR_Array :=
        LPWSTR_Pointers.Value
          (LPWSTR_Pointers.Pointer (Argv), Interfaces.C.ptrdiff_t (Argc));

   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. Args'Last loop
            Result.Append
              (VSS.Implementation.Windows.String_Utilities.From_Native_String
                 (Args (J)));
            null;
         end loop;

         VSS.Implementation.Windows.Kernel32.LocalFree (Argv);
      end return;
   end Arguments;

end Platform;
