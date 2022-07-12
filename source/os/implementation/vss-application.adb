--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Application is

   package Platform is

      function Arguments return VSS.String_Vectors.Virtual_String_Vector;
      --  Returns arguments provided in command line, except executable name.

      function Application_File return VSS.Strings.Virtual_String;
      --  Return path to executable file of the application.

   end Platform;

   package body Platform is separate;

   ----------------------
   -- Application_File --
   ----------------------

   function Application_File return VSS.Strings.Virtual_String
     renames Platform.Application_File;

   ---------------
   -- Arguments --
   ---------------

   function Arguments return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Platform.Arguments;
   end Arguments;

   ------------------------
   -- System_Environment --
   ------------------------

   function System_Environment return VSS.Environments.Process_Environment is
   begin
      return Result : VSS.Environments.Process_Environment;
   end System_Environment;

end VSS.Application;
