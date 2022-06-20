--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Content_Handlers;
private with VSS.JSON.Pull_Readers.Simple;
with VSS.Text_Streams;

package VSS.JSON.Push_Readers.Simple is

   type JSON_Simple_Push_Reader is
     limited new VSS.JSON.Push_Readers.JSON_Push_Reader
       with private;

   procedure Set_Stream
     (Self   : in out JSON_Simple_Push_Reader'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access);
   --  Set text stream to be used to obtain data.

   procedure Parse (Self : in out JSON_Simple_Push_Reader'Class);
   --  Parse all available data and return. It can be called again when
   --  non-blocking text stream is used and new data is available.

   function Has_Error (Self : JSON_Simple_Push_Reader'Class) return Boolean;
   --  Return True when some error is detected by the parser of reported
   --  by the handlers.

   function Error_Message
     (Self : JSON_Simple_Push_Reader'Class) return VSS.Strings.Virtual_String;
   --  Return text of the last detected error.

private

   type JSON_Simple_Push_Reader is
     limited new VSS.JSON.Push_Readers.JSON_Push_Reader
   with record
      Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Content : VSS.JSON.Content_Handlers.JSON_Content_Handler_Access;
      Error   : VSS.JSON.Pull_Readers.JSON_Reader_Error
        := VSS.JSON.Pull_Readers.No_Error;
      Message : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Set_Content_Handler
     (Self : in out JSON_Simple_Push_Reader;
      To   : VSS.JSON.Content_Handlers.JSON_Content_Handler_Access);
   --  Set content handler to process stream.

end VSS.JSON.Push_Readers.Simple;
