--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with VSS.Characters;
private with VSS.Strings;
with VSS.Text_Streams;

package Stdout_Text_Streams is

   type Output_Text_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with private;

private

   type Output_Text_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with record
      Message : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Put_Line
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure New_Line
     (Self    : in out Output_Text_Stream;
      Success : in out Boolean);

   overriding function Has_Error
     (Self : Output_Text_Stream) return Boolean is (not Self.Message.Is_Empty);

   overriding function Error_Message
     (Self : Output_Text_Stream) return VSS.Strings.Virtual_String
         is (Self.Message);

end Stdout_Text_Streams;
