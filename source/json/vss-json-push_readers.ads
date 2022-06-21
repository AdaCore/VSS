--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Content_Handlers;

package VSS.JSON.Push_Readers is

   pragma Preelaborate;

   type JSON_Push_Reader is limited interface;

   not overriding procedure Set_Content_Handler
     (Self : in out JSON_Push_Reader;
      To   : VSS.JSON.Content_Handlers.JSON_Content_Handler_Access)
      is abstract;
   --  Set content handler to process stream.

end VSS.JSON.Push_Readers;
