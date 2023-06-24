--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Abstract API of the JSON "pull" reader.

with VSS.Strings;

with VSS.JSON.Streams.Cursors;

package VSS.JSON.Pull_Readers is

   pragma Preelaborate;

   type JSON_Reader_Error is
     (No_Error,
      Custom_Error,
      Not_Valid,
      Premature_End_Of_Document);

   type JSON_Pull_Reader is limited interface
     and VSS.JSON.Streams.Cursors.JSON_Stream_Cursor;

   function At_End (Self : JSON_Pull_Reader) return Boolean is abstract;

   function Read_Next
     (Self : in out JSON_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is abstract;

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

   procedure Skip_Current_Array (Self : in out JSON_Pull_Reader) is abstract;

   procedure Skip_Current_Object
     (Self : in out JSON_Pull_Reader) is abstract;

   procedure Skip_Current_Value (Self : in out JSON_Pull_Reader) is abstract;

end VSS.JSON.Pull_Readers;
