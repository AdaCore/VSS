------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with VSS.JSON.Streams.Content_Handlers;
private with VSS.JSON.Pull_Readers.Simple;
with VSS.Text_Streams;

package VSS.JSON.Streams.Push.Readers.Simple is

   type JSON_Simple_Push_Reader is
     limited new VSS.JSON.Streams.Push.Readers.JSON_Push_Reader
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
     limited new VSS.JSON.Streams.Push.Readers.JSON_Push_Reader
   with record
      Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Content : VSS.JSON.Streams.Content_Handlers.JSON_Content_Handler_Access;
      Error   : VSS.JSON.Pull_Readers.JSON_Reader_Error
        := VSS.JSON.Pull_Readers.No_Error;
      Message : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Set_Content_Handler
     (Self : in out JSON_Simple_Push_Reader;
      To   : VSS.JSON.Streams.Content_Handlers.JSON_Content_Handler_Access);
   --  Set content handler to process stream.

end VSS.JSON.Streams.Push.Readers.Simple;
