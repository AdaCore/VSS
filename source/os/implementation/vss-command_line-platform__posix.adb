--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

separate (VSS.Command_Line)
package body Platform is

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Message : VSS.Strings.Virtual_String) is
   begin
      Output_Error (Message);
   end Report_Error;

end Platform;
