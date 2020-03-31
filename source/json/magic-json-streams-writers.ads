------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Interfaces;

with Magic.JSON.Streams.Content_Handlers;
with Magic.Strings;
with Magic.Text_Streams;

package Magic.JSON.Streams.Writers is

   type JSON_Simple_Writer is
     limited new Magic.JSON.Streams.Content_Handlers.JSON_Content_Handler
       with private;

   procedure Set_Stream
     (Self   : in out JSON_Simple_Writer'Class;
      Stream : not null Magic.Text_Streams.Output_Text_Stream_Access);
   --  Sets output text stream to be used to generate JSON document. Change of
   --  the stream is effective only before call to Start_Document.

   procedure Start_Document (Self : in out JSON_Simple_Writer'Class);

   procedure End_Document (Self : in out JSON_Simple_Writer'Class);

   procedure Start_Array (Self : in out JSON_Simple_Writer'Class);

   procedure End_Array (Self : in out JSON_Simple_Writer'Class);

   procedure Start_Object (Self : in out JSON_Simple_Writer'Class);

   procedure End_Object (Self : in out JSON_Simple_Writer'Class);

   procedure Key_Name
     (Self : in out JSON_Simple_Writer'Class;
      Name : Magic.Strings.Magic_String'Class);

   procedure String_Value
     (Self  : in out JSON_Simple_Writer'Class;
      Value : Magic.Strings.Magic_String'Class);

   procedure Integer_Value
     (Self  : in out JSON_Simple_Writer'Class;
      Value : Interfaces.Integer_64);

   procedure Float_Value
     (Self  : in out JSON_Simple_Writer'Class;
      Value : Interfaces.IEEE_Float_64);

   procedure Boolean_Value
     (Self  : in out JSON_Simple_Writer'Class;
      Value : Boolean);

   procedure Null_Value (Self : in out JSON_Simple_Writer'Class);

   overriding procedure Start_Document
     (Self : in out JSON_Simple_Writer; Success : in out Boolean);

   overriding procedure End_Document
     (Self : in out JSON_Simple_Writer; Success : in out Boolean);

   overriding procedure Start_Array
     (Self : in out JSON_Simple_Writer; Success : in out Boolean);

   overriding procedure End_Array
     (Self : in out JSON_Simple_Writer; Success : in out Boolean);

   overriding procedure Start_Object
     (Self : in out JSON_Simple_Writer; Success : in out Boolean);

   overriding procedure End_Object
     (Self : in out JSON_Simple_Writer; Success : in out Boolean);

   overriding procedure Key_Name
     (Self    : in out JSON_Simple_Writer;
      Name    : Magic.Strings.Magic_String'Class;
      Success : in out Boolean);

   overriding procedure String_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Magic.Strings.Magic_String'Class;
      Success : in out Boolean);

   overriding procedure Integer_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Interfaces.Integer_64;
      Success : in out Boolean);

   overriding procedure Float_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Interfaces.IEEE_Float_64;
      Success : in out Boolean);

   overriding procedure Boolean_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Boolean;
      Success : in out Boolean);

   overriding procedure Null_Value
     (Self : in out JSON_Simple_Writer; Success : in out Boolean);

private

   type JSON_Simple_Writer is
     limited new Magic.JSON.Streams.Content_Handlers.JSON_Content_Handler
   with record
      Configured_Stream : Magic.Text_Streams.Output_Text_Stream_Access;
      Effective_Stream  : Magic.Text_Streams.Output_Text_Stream_Access;
      Open_Parenthesis  : Boolean := False;
   end record;

end Magic.JSON.Streams.Writers;
