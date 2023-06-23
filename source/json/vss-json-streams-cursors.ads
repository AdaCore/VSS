--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Strings;

package VSS.JSON.Streams.Cursors is

   pragma Preelaborate;

   type JSON_Stream_Cursor is limited interface;

   function Element_Kind
     (Self : JSON_Stream_Cursor) return JSON_Stream_Element_Kind is abstract;

   function Is_Start_Document (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_End_Document (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_Start_Array (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_End_Array (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_Start_Object (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_End_Object (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_Key_Name (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_String_Value (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_Number_Value (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_Boolean_Value (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Is_Null_Value (Self : JSON_Stream_Cursor'Class) return Boolean;

   function Key_Name
     (Self : JSON_Stream_Cursor) return VSS.Strings.Virtual_String is abstract;

   function String_Value
     (Self : JSON_Stream_Cursor) return VSS.Strings.Virtual_String is abstract;

   function Number_Value
     (Self : JSON_Stream_Cursor) return VSS.JSON.JSON_Number is abstract;

   function Boolean_Value
     (Self : JSON_Stream_Cursor) return Boolean is abstract;

   function Element
     (Self : JSON_Stream_Cursor'Class)
      return VSS.JSON.Streams.JSON_Stream_Element;

end VSS.JSON.Streams.Cursors;
