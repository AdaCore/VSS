--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

separate (VSS.Environments)
package body Platform is

   ---------------------------
   -- Native_Path_Separator --
   ---------------------------

   function Native_Path_Separator return VSS.Characters.Virtual_Character is
   begin
      return ':';
   end Native_Path_Separator;

end Platform;
