--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Environments;
with VSS.Strings;
with VSS.String_Vectors;

package VSS.Application is

   function Application_File return VSS.Strings.Virtual_String;
   --  Return path to application's executable file

   function Arguments return VSS.String_Vectors.Virtual_String_Vector;
   --  Return arguments of the application

   function System_Environment return VSS.Environments.Process_Environment;
   --  Return system environment of the application

end VSS.Application;
