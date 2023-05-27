--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Common code for implementation of JSON parsers.

with VSS.JSON.Pull_Readers;
with VSS.Text_Streams;

package VSS.JSON.Implementation.Parsers is

   pragma Preelaborate;

   type JSON_Parser_Base is abstract tagged limited private;

   procedure Set_Stream
     (Self   : in out JSON_Parser_Base'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access);

   function Event_Kind
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Pull_Readers.JSON_Event_Kind;
   --  Return current event.

   function Error
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error;
   --  Return current error.

   function Error_Message
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String;
   --  Return error message.

   function String_Value
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String;
   --  Return string data (key name or string value)

   function Boolean_Value (Self : JSON_Parser_Base'Class) return Boolean;
   --  Return boolean value

   function Number_Value
     (Self : JSON_Parser_Base'Class) return VSS.JSON.JSON_Number;
   --  Return number value

private

   type JSON_Parser_Base is abstract tagged limited record
      Stream  : VSS.Text_Streams.Input_Text_Stream_Access;

      Error   : VSS.JSON.Pull_Readers.JSON_Reader_Error :=
        VSS.JSON.Pull_Readers.No_Error;
      Message : VSS.Strings.Virtual_String;

      Event   : VSS.JSON.Pull_Readers.JSON_Event_Kind :=
        VSS.JSON.Pull_Readers.No_Token;
      Buffer  : VSS.Strings.Virtual_String;
      Boolean : Standard.Boolean;
      Number  : VSS.JSON.JSON_Number;
   end record;

end VSS.JSON.Implementation.Parsers;
