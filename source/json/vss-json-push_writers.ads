--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Content_Handlers;
private with VSS.Strings;
with VSS.Text_Streams;

package VSS.JSON.Push_Writers is

   type JSON_Simple_Push_Writer is
     limited new VSS.JSON.Content_Handlers.JSON_Content_Handler
       with private;

   procedure Set_Stream
     (Self   : in out JSON_Simple_Push_Writer'Class;
      Stream : not null VSS.Text_Streams.Output_Text_Stream_Access);
   --  Sets output text stream to be used to generate JSON document. Change of
   --  the stream is effective only before call to Start_Document.

private

   type JSON_Simple_Push_Writer is
     limited new VSS.JSON.Content_Handlers.JSON_Content_Handler
   with record
      Configured_Stream : VSS.Text_Streams.Output_Text_Stream_Access;
      Effective_Stream  : VSS.Text_Streams.Output_Text_Stream_Access;
      Open_Parenthesis  : Boolean := False;
   end record;

   overriding procedure Start_Document
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean);

   overriding procedure End_Document
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean);

   overriding procedure Start_Array
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean);

   overriding procedure End_Array
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean);

   overriding procedure Start_Object
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean);

   overriding procedure End_Object
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean);

   overriding procedure Key_Name
     (Self    : in out JSON_Simple_Push_Writer;
      Name    : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean);

   overriding procedure String_Value
     (Self    : in out JSON_Simple_Push_Writer;
      Value   : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean);

   overriding procedure Number_Value
     (Self    : in out JSON_Simple_Push_Writer;
      Value   : VSS.JSON.JSON_Number;
      Success : in out Boolean);

   overriding procedure Boolean_Value
     (Self    : in out JSON_Simple_Push_Writer;
      Value   : Boolean;
      Success : in out Boolean);

   overriding procedure Null_Value
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean);

   overriding function Error_Message
     (Self : JSON_Simple_Push_Writer) return VSS.Strings.Virtual_String;

end VSS.JSON.Push_Writers;
