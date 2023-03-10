--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Wide_Wide_Text_IO;

package body Stdout_Text_Streams is

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self    : in out Output_Text_Stream;
      Success : in out Boolean) is
   begin
      raise Program_Error;
   end New_Line;

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
         Self.Message := "exception raised by IO";
         Success      := False;
   end Put;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      raise Program_Error;
   end Put;

   --------------
   -- Put_Line --
   --------------

   overriding procedure Put_Line
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      raise Program_Error;
   end Put_Line;

end Stdout_Text_Streams;
