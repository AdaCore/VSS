--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Finalization;
private with Interfaces.C_Streams;

private with VSS.Strings.Converters.Decoders;

package VSS.Text_Streams.File_Input is

   type File_Input_Text_Stream is
     limited new VSS.Text_Streams.Input_Text_Stream with private;

   procedure Open
     (Self : in out File_Input_Text_Stream'Class;
      Name : VSS.Strings.Virtual_String);
   --  Open file with the given name to load text data from. System defined
   --  encoding is used to decode text data.

   procedure Open
     (Self     : in out File_Input_Text_Stream'Class;
      Name     : VSS.Strings.Virtual_String;
      Encoding : VSS.Strings.Virtual_String);
   --  Open file with the given name to load text data encoded with given text
   --  encoding.

   procedure Set_Encoding
     (Self     : in out File_Input_Text_Stream'Class;
      Encoding : VSS.Strings.Virtual_String);
   --  Sets encoding of the data to be used to load information from the file.
   --  Encoding can be set only when file is not open.

   procedure Close (Self : in out File_Input_Text_Stream'Class);
   --  Close file.

private

   type File_Input_Text_Stream is
     limited new Ada.Finalization.Limited_Controlled
       and VSS.Text_Streams.Input_Text_Stream with
   record
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      Stream  : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.NULL_Stream;
      Buffer  : VSS.Strings.Virtual_String;
      Error   : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Get
     (Self    : in out File_Input_Text_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding function Is_End_Of_Data
     (Self : File_Input_Text_Stream) return Boolean;

   overriding function Is_End_Of_Stream
     (Self : File_Input_Text_Stream) return Boolean;

   overriding function Has_Error
     (Self : File_Input_Text_Stream) return Boolean;

   overriding function Error_Message
     (Self : File_Input_Text_Stream) return VSS.Strings.Virtual_String;

end VSS.Text_Streams.File_Input;
