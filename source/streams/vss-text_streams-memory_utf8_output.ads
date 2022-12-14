--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   overriding function Has_Error
     (Self : Memory_UTF8_Output_Stream) return Boolean;

   overriding function Error_Message
     (Self : Memory_UTF8_Output_Stream) return VSS.Strings.Virtual_String;

end VSS.Text_Streams.Memory_UTF8_Output;
