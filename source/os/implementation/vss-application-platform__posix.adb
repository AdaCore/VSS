--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
     with Import, Convention => C, External_Name => "gnat_argc";
   GNAT_Argv : constant chars_ptr_Pointers.Pointer
     with Import, Convention => C, External_Name => "gnat_argv";

   ----------------------
   -- Application_File --
   ----------------------

   function Application_File return VSS.Strings.Virtual_String is separate;

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
