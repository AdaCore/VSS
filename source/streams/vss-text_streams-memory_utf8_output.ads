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

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

end VSS.Text_Streams.Memory_UTF8_Output;
