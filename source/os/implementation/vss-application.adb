--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Application is

   package Platform is

      function Arguments return VSS.String_Vectors.Virtual_String_Vector;
      --  Returns arguments provided in command line, except executable name.

   end Platform;

   package body Platform is separate;

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
