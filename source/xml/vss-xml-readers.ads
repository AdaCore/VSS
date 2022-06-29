--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.XML.Content_Handlers;
with VSS.XML.Error_Handlers;
with VSS.XML.Lexical_Handlers;

package VSS.XML.Readers is

   pragma Preelaborate;

   type SAX_Reader is limited interface;

   procedure Set_Content_Handler
     (Self    : in out SAX_Reader;
      Handler : VSS.XML.Content_Handlers.SAX_Content_Handler_Access)
        is abstract;

   procedure Set_Lexical_Handler
     (Self    : in out SAX_Reader;
      Handler : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access)
        is abstract;

   procedure Set_Error_Handler
     (Self    : in out SAX_Reader;
      Handler : VSS.XML.Error_Handlers.SAX_Error_Handler_Access)
        is abstract;

end VSS.XML.Readers;
