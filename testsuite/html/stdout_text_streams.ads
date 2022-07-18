--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with VSS.Characters;
with VSS.Text_Streams;

package Stdout_Text_Streams is

   type Output_Text_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with private;

private

   type Output_Text_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with null record;

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

end Stdout_Text_Streams;
