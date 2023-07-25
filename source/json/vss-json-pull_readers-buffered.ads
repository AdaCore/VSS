--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

private with Ada.Containers.Vectors;

package VSS.JSON.Pull_Readers.Buffered is

   pragma Preelaborate;

   type JSON_Buffered_Pull_Reader
     (Reader : not null access JSON_Pull_Reader'Class)
        is limited new JSON_Pull_Reader with private;

   procedure Mark (Self : in out JSON_Buffered_Pull_Reader'Class);
   --  Mark position in the input stream. Call of the Reset subprogram
   --  reposition stream to the marked position.

   procedure Reset (Self : in out JSON_Buffered_Pull_Reader'Class);
   --  Reposition stream to the last marked position.

   procedure Unmark (Self : in out JSON_Buffered_Pull_Reader'Class);
   --  Remove position mark.

   overriding function Key_Name
     (Self : JSON_Buffered_Pull_Reader) return VSS.Strings.Virtual_String;
   --  GNAT 20230603: subprogram is invisible when declared in the private
   --  part.

private

   package JSON_Stream_Element_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => VSS.JSON.Streams.JSON_Stream_Element,
        "="          => VSS.JSON.Streams."=");

   type JSON_Buffered_Pull_Reader
     (Reader : not null access JSON_Pull_Reader'Class)
   is limited new JSON_Pull_Reader with record
      Buffer  : JSON_Stream_Element_Vectors.Vector;
      Current : Natural := 0;
      Store   : Boolean := False;
   end record with Preelaborable_Initialization;

   overriding function Element_Kind
     (Self : JSON_Buffered_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind;

   overriding function String_Value
     (Self : JSON_Buffered_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding function Number_Value
     (Self : JSON_Buffered_Pull_Reader) return VSS.JSON.JSON_Number;

   overriding function Boolean_Value
     (Self : JSON_Buffered_Pull_Reader) return Boolean;

   overriding function At_End
     (Self : JSON_Buffered_Pull_Reader) return Boolean;

   overriding function Read_Next
     (Self : in out JSON_Buffered_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind;

   overriding procedure Clear (Self : in out JSON_Buffered_Pull_Reader);

   overriding function Error
     (Self : JSON_Buffered_Pull_Reader) return JSON_Reader_Error;

   overriding function Error_Message
     (Self : JSON_Buffered_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding procedure Raise_Error
     (Self    : in out JSON_Buffered_Pull_Reader;
      Message : VSS.Strings.Virtual_String);

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Buffered_Pull_Reader);

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Buffered_Pull_Reader);

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Buffered_Pull_Reader);

end VSS.JSON.Pull_Readers.Buffered;
