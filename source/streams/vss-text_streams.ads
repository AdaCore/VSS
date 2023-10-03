--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Abstract API of the streams interpreted as sequence of characters.

with VSS.Characters;
with VSS.Strings;

package VSS.Text_Streams is

   pragma Preelaborate;

   type Input_Text_Stream is limited interface;

   type Input_Text_Stream_Access is access all Input_Text_Stream'Class;

   procedure Get
     (Self    : in out Input_Text_Stream;
      Item    : out VSS.Characters.Virtual_Character'Base;
      Success : in out Boolean) is abstract;
   --  Get character from the input stream.
   --
   --  @param Self     Text stream itself
   --  @param Item
   --    Buffer to store character. It should be set to invalid value in case
   --  of stream error or unavailable data
   --  @param Success
   --    Subprogram do nothing when False, and set it to False on failure.

   function Is_End_Of_Data
     (Self : Input_Text_Stream) return Boolean is abstract;
   --  Return True when there is no more data is available now.

   function Is_End_Of_Stream
     (Self : Input_Text_Stream) return Boolean is abstract;
   --  Return True when when end of stream is reached.

   function Has_Error (Self : Input_Text_Stream) return Boolean is abstract;
   --  Return True when any error is detected.

   function Error_Message
     (Self : Input_Text_Stream) return VSS.Strings.Virtual_String is abstract;
   --  Return error message when Has_Error returns True, or 'null' string.

   type Output_Text_Stream is limited interface;

   type Output_Text_Stream_Access is access all Output_Text_Stream'Class;

   procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean) is abstract;
   --  Output character to stream with conversion to the stream's text
   --  encoding.
   --
   --  @param Self     Text stream itself
   --  @param Item     Character to output
   --  @param Success
   --    Subprogram do nothing when False, and set it to False on failure.

   procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is abstract;
   --  Output string to stream with conversion to the stream's text encoding.
   --
   --  @param Self     Text stream itself
   --  @param Item     String to output
   --  @param Success
   --    Subprogram do nothing when False, and set it to False on failure.

   procedure Put_Line
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is abstract;
   --  Output string to stream with conversion to the stream's text encoding
   --  and output line terminator sequence.
   --
   --  @param Self     Text stream itself
   --  @param Item     String to output
   --  @param Success
   --    Subprogram do nothing when False, and set it to False on failure.

   procedure New_Line
     (Self    : in out Output_Text_Stream;
      Success : in out Boolean) is abstract;
   --  Output line terminator sequence to the text stream.
   --
   --  @param Self     Text stream itself
   --  @param Success
   --    Subprogram do nothing when False, and set it to False on failure.

   function Has_Error (Self : Output_Text_Stream) return Boolean is abstract;
   --  Return True when any error is detected.

   function Error_Message
     (Self : Output_Text_Stream) return VSS.Strings.Virtual_String
         is abstract;
   --  Return error message when Has_Error returns True, or 'null' string.

end VSS.Text_Streams;
