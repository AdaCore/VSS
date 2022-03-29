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
--  This is GNAT specific package, it use symbols declared in argv.c file
--  of the GNAT Run Time Library.
--
--  Note: only UTF-8 locales are supported.

with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with VSS.Strings.Conversions;

separate (VSS.Application)
package body Platform is

   type chars_ptr_Array is
     array (Natural range <>) of aliased Interfaces.C.Strings.chars_ptr;

   package chars_ptr_Pointers is
     new Interfaces.C.Pointers
       (Natural,
        Interfaces.C.Strings.chars_ptr,
        chars_ptr_Array,
        Interfaces.C.Strings.Null_Ptr);

   GNAT_Argc : constant Interfaces.C.int
     with Import, Convention => C, Link_Name => "gnat_argc";
   GNAT_Argv : constant chars_ptr_Pointers.Pointer
     with Import, Convention => C, Link_Name => "gnat_argv";

   ---------------
   -- Arguments --
   ---------------

   function Arguments return VSS.String_Vectors.Virtual_String_Vector is
      Args : constant chars_ptr_Array :=
        chars_ptr_Pointers.Value
          (GNAT_Argv, Interfaces.C.ptrdiff_t (GNAT_Argc));

   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for Index in 1 .. Args'Last loop
            Result.Append
              (VSS.Strings.Conversions.To_Virtual_String
                 (Interfaces.C.Strings.Value (Args (Index))));
            --  XXX Locale specific converter should be used here.
         end loop;
      end return;
   end Arguments;

end Platform;
