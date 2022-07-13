--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  XXX It is dummy stub only. _NSGetExecutablePath must be used here.

separate (VSS.Application.Platform)
function Application_File return VSS.Strings.Virtual_String is
   Args : constant chars_ptr_Array :=
     chars_ptr_Pointers.Value
       (GNAT_Argv, Interfaces.C.ptrdiff_t (GNAT_Argc));

begin
   return
     VSS.Strings.Conversions.To_Virtual_String
       (Interfaces.C.Strings.Value (Args (0)));
end Application_File;
