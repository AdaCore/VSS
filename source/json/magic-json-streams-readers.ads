------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--  Abstract API of the JSON "push" reader.

with Magic.Strings;

package Magic.JSON.Streams.Readers is

   pragma Preelaborate;

   type JSON_Event_Kind is
     (No_Token,
      Invalid,
      Start_Document,
      End_Document,
      Start_Array,
      End_Array,
      Start_Object,
      End_Object,
      Key_Name,
      String_Value,
      Number_Value,
      Boolean_Value,
      Null_Value);

   type JSON_Reader_Error is
     (No_Error,
      Custom_Error,
      Not_Valid,
      Premature_End_Of_Document);

   type JSON_Stream_Reader is limited interface;

   function At_End (Self : JSON_Stream_Reader) return Boolean is abstract;

   function Read_Next
     (Self : in out JSON_Stream_Reader) return JSON_Event_Kind is abstract;

   procedure Read_Next (Self : in out JSON_Stream_Reader'Class);

   procedure Clear (Self : in out JSON_Stream_Reader) is abstract;

   function Error
     (Self : JSON_Stream_Reader) return JSON_Reader_Error is abstract;

   function Error_Message
     (Self : JSON_Stream_Reader) return Magic.Strings.Magic_String is abstract;

   function Has_Error (Self : JSON_Stream_Reader'Class) return Boolean;

   procedure Raise_Error
     (Self    : in out JSON_Stream_Reader;
      Message : Magic.Strings.Magic_String := Magic.Strings.Empty_Magic_String)
   is abstract;

   function Event_Kind
     (Self : JSON_Stream_Reader) return JSON_Event_Kind is abstract;

   function Is_Start_Document (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_End_Document (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_Start_Array (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_End_Array (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_Start_Object (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_End_Object (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_Key_Name (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_String_Value (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_Number_Value (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_Boolean_Value (Self : JSON_Stream_Reader'Class) return Boolean;

   function Is_Null_Value (Self : JSON_Stream_Reader'Class) return Boolean;

   function Key_Name
     (Self : JSON_Stream_Reader) return Magic.Strings.Magic_String is abstract;

   function String_Value
     (Self : JSON_Stream_Reader) return Magic.Strings.Magic_String is abstract;

   function Number_Value
     (Self : JSON_Stream_Reader) return Magic.JSON.JSON_Number is abstract;

   function Boolean_Value
     (Self : JSON_Stream_Reader) return Boolean is abstract;

   procedure Skip_Current_Array (Self : in out JSON_Stream_Reader) is abstract;

   procedure Skip_Current_Object (Self : in out JSON_Stream_Reader) is abstract;

   procedure Skip_Current_Value (Self : in out JSON_Stream_Reader) is abstract;

end Magic.JSON.Streams.Readers;
