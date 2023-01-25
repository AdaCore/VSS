--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Implementation of the output text stream to output text to the files.

private with Ada.Finalization;
private with Interfaces.C_Streams;

private with VSS.Strings.Converters.Encoders;

package VSS.Text_Streams.File_Output is

   type File_Output_Text_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with private;

   procedure Create
     (Self : in out File_Output_Text_Stream'Class;
      Name : VSS.Strings.Virtual_String'Class);
   --  Create file with given name to output text data into it. System defined
   --  encoding is used to encode text data.

   procedure Create
     (Self     : in out File_Output_Text_Stream'Class;
      Name     : VSS.Strings.Virtual_String'Class;
      Encoding : VSS.Strings.Virtual_String);
   --  Create file with given name to output text data into it using given
   --  text encoding.

   procedure Set_Encoding
     (Self     : in out File_Output_Text_Stream'Class;
      Encoding : VSS.Strings.Virtual_String);
   --  Sets encoding of the data to be used to save information into the file.
   --  Encoding can be set only when file is not open.

   procedure Close (Self : in out File_Output_Text_Stream'Class);
   --  Close file.

private

   type File_Output_Text_Stream is
     limited new Ada.Finalization.Limited_Controlled
       and VSS.Text_Streams.Output_Text_Stream with
   record
      Encoder    : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
      Terminator : VSS.Strings.Line_Terminator := VSS.Strings.LF;
      Stream     : Interfaces.C_Streams.FILEs :=
        Interfaces.C_Streams.NULL_Stream;
      Error      : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Finalize (Self : in out File_Output_Text_Stream);

   overriding procedure Put
     (Self    : in out File_Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding procedure Put
     (Self    : in out File_Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Put_Line
     (Self    : in out File_Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure New_Line
     (Self    : in out File_Output_Text_Stream;
      Success : in out Boolean);

   overriding function Has_Error
     (Self : File_Output_Text_Stream) return Boolean;

   overriding function Error_Message
     (Self : File_Output_Text_Stream) return VSS.Strings.Virtual_String;

end VSS.Text_Streams.File_Output;
