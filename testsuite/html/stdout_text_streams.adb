--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Wide_Wide_Text_IO;

package body Stdout_Text_Streams is

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean) is
   begin
      Ada.Wide_Wide_Text_IO.Put (Wide_Wide_Character (Item));

   exception
      when others =>
         Success := False;
   end Put;

end Stdout_Text_Streams;
