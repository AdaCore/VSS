--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Streams;

private with VSS.Implementation.UTF8_Encoding;
with VSS.Stream_Element_Vectors;

package VSS.Text_Streams.Memory_UTF8_Input is

   type Memory_UTF8_Input_Stream is
     limited new VSS.Text_Streams.Input_Text_Stream with private;

   procedure Set_Data
     (Self : in out Memory_UTF8_Input_Stream'Class;
      Data : VSS.Stream_Element_Vectors.Stream_Element_Vector);
   --  Set data to be processed.

   procedure Rewind (Self : in out Memory_UTF8_Input_Stream'Class);
   --  Move current position to be processed to the begin of the data.

private

   type Memory_UTF8_Input_Stream is
     limited new VSS.Text_Streams.Input_Text_Stream with record
      Buffer  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Current : Ada.Streams.Stream_Element_Count := 1;
      Error   : VSS.Implementation.UTF8_Encoding.UTF8_Decode_Error :=
        VSS.Implementation.UTF8_Encoding.None;
   end record;

   overriding procedure Get
     (Self    : in out Memory_UTF8_Input_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding function Is_End_Of_Data
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Is_End_Of_Stream
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Has_Error
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Error_Message
     (Self : Memory_UTF8_Input_Stream) return VSS.Strings.Virtual_String;

end VSS.Text_Streams.Memory_UTF8_Input;
