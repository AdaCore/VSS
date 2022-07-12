--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.UTF_Encoding;
with Interfaces.C;

with VSS.Strings.Conversions;

separate (VSS.Application.Platform)
function Application_File return VSS.Strings.Virtual_String is

   use type Interfaces.C.char_array;
   use type Interfaces.C.size_t;

   proc_self_exe_Path : constant Interfaces.C.char_array :=
     "/proc/self/exe" & Interfaces.C.nul;

   function readlink
     (pathname : not null access constant Interfaces.C.char;
      buf      : not null access Interfaces.C.char;
      bufsiz   : Interfaces.C.size_t) return Interfaces.C.size_t
     with Import, Convention => C, External_Name => "readlink";

   Size : Interfaces.C.size_t := 512;

begin
   loop
      declare
         Buffer : Interfaces.C.char_array (1 .. Size);
         Result : Interfaces.C.size_t;

      begin
         Result :=
           readlink
             (proc_self_exe_Path (proc_self_exe_Path'First)'Access,
              Buffer (Buffer'First)'Access,
              Size);

         if Result = Interfaces.C.size_t'Last then
            --  'readlink' returns -1, it means failure of read of the
            --  symbolic link, so fallback to the first element of the
            --  'argv' vector.

            declare
               Args : constant chars_ptr_Array :=
                 chars_ptr_Pointers.Value
                   (GNAT_Argv, Interfaces.C.ptrdiff_t (GNAT_Argc));

            begin
               return
                 VSS.Strings.Conversions.To_Virtual_String
                   (Interfaces.C.Strings.Value (Args (0)));
            end;

         elsif Result < Size then
            declare
               Path : Ada.Strings.UTF_Encoding.UTF_8_String
                 (1 .. Integer (Result))
                   with Import, Address => Buffer (Buffer'First)'Address;

            begin
               return
                 VSS.Strings.Conversions.To_Virtual_String (Path);
               --  ??? Locale specific encoding must be used here.
            end;
         end if;

         Size := Size * 2;
      end;
   end loop;
end Application_File;
