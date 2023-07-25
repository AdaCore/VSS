--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Utilities to support JSON testsuite.

with Ada.Containers.Vectors;

with VSS.JSON.Pull_Readers;
with VSS.JSON.Streams;
with VSS.Strings;

package Tests_JSON_Streams is

   package JSON_Stream_Element_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => VSS.JSON.Streams.JSON_Stream_Element,
        "="          => VSS.JSON.Streams."=");
   --  For testing purpose, this vector can contain any kind of elements,
   --  including special parser states (incomplete/error).

   type Replay_Pull_Reader
     (Data : not null access constant JSON_Stream_Element_Vectors.Vector'Class)
   is limited new VSS.JSON.Pull_Readers.JSON_Pull_Reader with private;
   --  Pull reader to sequentially iterate over elements in Data vector.

   procedure Initialize (Self : in out Replay_Pull_Reader);

private

   type Replay_Pull_Reader
     (Data : not null access constant JSON_Stream_Element_Vectors.Vector'Class)
   is limited new VSS.JSON.Pull_Readers.JSON_Pull_Reader with record
      Current : Natural := 0;
   end record;

   overriding function Element_Kind
     (Self : Replay_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind;

   overriding function Key_Name
     (Self : Replay_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding function String_Value
     (Self : Replay_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding function Number_Value
     (Self : Replay_Pull_Reader) return VSS.JSON.JSON_Number;

   overriding function Boolean_Value
     (Self : Replay_Pull_Reader) return Boolean;

   overriding function At_End (Self : Replay_Pull_Reader) return Boolean;

   overriding function Read_Next
     (Self : in out Replay_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind;

   overriding procedure Clear (Self : in out Replay_Pull_Reader);

   overriding function Error
     (Self : Replay_Pull_Reader)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error;

   overriding function Error_Message
     (Self : Replay_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding procedure Raise_Error
     (Self    : in out Replay_Pull_Reader;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);

   overriding procedure Skip_Current_Array (Self : in out Replay_Pull_Reader);

   overriding procedure Skip_Current_Object
     (Self : in out Replay_Pull_Reader);

   overriding procedure Skip_Current_Value (Self : in out Replay_Pull_Reader);

end Tests_JSON_Streams;
