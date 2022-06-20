--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Abstract API of the JSON "pull" reader.

with VSS.Strings;

package VSS.JSON.Pull_Readers is

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

   type JSON_Pull_Reader is limited interface;

   function At_End (Self : JSON_Pull_Reader) return Boolean is abstract;

   function Read_Next
     (Self : in out JSON_Pull_Reader) return JSON_Event_Kind is abstract;

   procedure Read_Next (Self : in out JSON_Pull_Reader'Class);

   procedure Clear (Self : in out JSON_Pull_Reader) is abstract;

   function Error
     (Self : JSON_Pull_Reader) return JSON_Reader_Error is abstract;

   function Error_Message
     (Self : JSON_Pull_Reader) return VSS.Strings.Virtual_String is abstract;

   function Has_Error (Self : JSON_Pull_Reader'Class) return Boolean;

   procedure Raise_Error
     (Self    : in out JSON_Pull_Reader;
      Message : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String)
   is abstract;

   function Event_Kind
     (Self : JSON_Pull_Reader) return JSON_Event_Kind is abstract;

   function Is_Start_Document (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_End_Document (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_Start_Array (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_End_Array (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_Start_Object (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_End_Object (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_Key_Name (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_String_Value (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_Number_Value (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_Boolean_Value (Self : JSON_Pull_Reader'Class) return Boolean;

   function Is_Null_Value (Self : JSON_Pull_Reader'Class) return Boolean;

   function Key_Name
     (Self : JSON_Pull_Reader) return VSS.Strings.Virtual_String is abstract;

   function String_Value
     (Self : JSON_Pull_Reader) return VSS.Strings.Virtual_String is abstract;

   function Number_Value
     (Self : JSON_Pull_Reader) return VSS.JSON.JSON_Number is abstract;

   function Boolean_Value
     (Self : JSON_Pull_Reader) return Boolean is abstract;

   procedure Skip_Current_Array (Self : in out JSON_Pull_Reader) is abstract;

   procedure Skip_Current_Object
     (Self : in out JSON_Pull_Reader) is abstract;

   procedure Skip_Current_Value (Self : in out JSON_Pull_Reader) is abstract;

end VSS.JSON.Pull_Readers;
