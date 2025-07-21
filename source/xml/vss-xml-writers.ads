--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.XML.Content_Handlers;
with VSS.XML.Error_Handlers;
with VSS.XML.Lexical_Handlers;

with VSS.Text_Streams;

package VSS.XML.Writers
  with Preelaborate
is

   type XML_Writer is limited interface
     and VSS.XML.Content_Handlers.SAX_Content_Handler
     and VSS.XML.Lexical_Handlers.SAX_Lexical_Handler;

   procedure Set_Output_Stream
     (Self   : in out XML_Writer;
      Stream : VSS.Text_Streams.Output_Text_Stream_Access) is abstract;

   procedure Set_Error_Handler
     (Self : in out XML_Writer;
      To   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access) is abstract;

end VSS.XML.Writers;
