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
--  This package declares interface type to be used as abstract interface to
--  process JSON streams in callback style API.

with Interfaces;

with Magic.Strings;

package Magic.JSON.Streams.Content_Handlers is

   pragma Preelaborate;

   type JSON_Content_Handler is limited interface;

   procedure Start_Document
     (Self : in out JSON_Content_Handler; Success : in out Boolean) is null;
   --  Called when processing of JSON document has been started

   procedure End_Document
     (Self : in out JSON_Content_Handler; Success : in out Boolean) is null;
   --  Called when processing of JSON document has need finished with any
   --  reason (document processed completely, document is invalid, processing
   --  is terminated by application). No other subprograms will be called
   --  before new call of Start_Document.

   procedure Start_Array
     (Self : in out JSON_Content_Handler; Success : in out Boolean) is null;

   procedure End_Array
     (Self : in out JSON_Content_Handler; Success : in out Boolean) is null;

   procedure Start_Object
     (Self : in out JSON_Content_Handler; Success : in out Boolean) is null;

   procedure End_Object
     (Self : in out JSON_Content_Handler; Success : in out Boolean) is null;

   procedure Key_Name
     (Self    : in out JSON_Content_Handler;
      Name    : Magic.Strings.Magic_String'Class;
      Success : in out Boolean) is null;

   procedure String_Value
     (Self    : in out JSON_Content_Handler;
      Value   : Magic.Strings.Magic_String'Class;
      Success : in out Boolean) is null;

   procedure Integer_Value
     (Self    : in out JSON_Content_Handler;
      Value   : Interfaces.Integer_64;
      Success : in out Boolean) is null;

   procedure Float_Value
     (Self    : in out JSON_Content_Handler;
      Value   : Interfaces.IEEE_Float_64;
      Success : in out Boolean) is null;

   procedure Boolean_Value
     (Self    : in out JSON_Content_Handler;
      Value   : Boolean;
      Success : in out Boolean) is null;

   procedure Null_Value
     (Self : in out JSON_Content_Handler; Success : in out Boolean) is null;

end Magic.JSON.Streams.Content_Handlers;
