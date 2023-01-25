--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Simple implementation of the text stream to store output in the memory as
--  seqeunce of stream elements in UTF-8 text encoding and using LF as line
--  separator.

with VSS.Stream_Element_Vectors;

package VSS.Text_Streams.Memory_UTF8_Output is

   type Memory_UTF8_Output_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with record
      Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
   end record;

private

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Put_Line
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure New_Line
     (Self    : in out Memory_UTF8_Output_Stream;
      Success : in out Boolean);

   overriding function Has_Error
     (Self : Memory_UTF8_Output_Stream) return Boolean;

   overriding function Error_Message
     (Self : Memory_UTF8_Output_Stream) return VSS.Strings.Virtual_String;

end VSS.Text_Streams.Memory_UTF8_Output;
