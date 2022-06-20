--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with VSS.JSON.Implementation.Parsers;
with VSS.Text_Streams;

package VSS.JSON.Pull_Readers.Simple is

   type JSON_Simple_Pull_Reader is limited new JSON_Pull_Reader with private;

   procedure Set_Stream
     (Self   : in out JSON_Simple_Pull_Reader'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access);

private

   type JSON_Simple_Pull_Reader is limited new JSON_Pull_Reader with record
      Parser : VSS.JSON.Implementation.Parsers.JSON_Parser;
   end record;

   overriding function At_End (Self : JSON_Simple_Pull_Reader) return Boolean;

   overriding function Boolean_Value
     (Self : JSON_Simple_Pull_Reader) return Boolean;

   overriding procedure Clear (Self : in out JSON_Simple_Pull_Reader);

   overriding function Error
     (Self : JSON_Simple_Pull_Reader)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error;

   overriding function Error_Message
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding function Event_Kind
     (Self : JSON_Simple_Pull_Reader)
      return VSS.JSON.Pull_Readers.JSON_Event_Kind;

   overriding function Key_Name
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding function Number_Value
     (Self : JSON_Simple_Pull_Reader) return VSS.JSON.JSON_Number;

   overriding procedure Raise_Error
     (Self    : in out JSON_Simple_Pull_Reader;
      Message : VSS.Strings.Virtual_String);

   overriding function Read_Next
     (Self : in out JSON_Simple_Pull_Reader)
      return VSS.JSON.Pull_Readers.JSON_Event_Kind;

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Simple_Pull_Reader);

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Simple_Pull_Reader);

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Simple_Pull_Reader);

   overriding function String_Value
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String;

end VSS.JSON.Pull_Readers.Simple;
