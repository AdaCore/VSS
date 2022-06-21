--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.Implementation.Environment_Utilities is

   function Get_Env
     (Name    : VSS.Strings.Virtual_String;
      Default : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
      return VSS.Strings.Virtual_String;
   --  Return value of the given environment variable,

end VSS.Implementation.Environment_Utilities;
