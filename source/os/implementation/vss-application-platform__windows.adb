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
