--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Characters;
with VSS.Strings;

package VSS.Text_Streams is

   pragma Preelaborate;

   type Input_Text_Stream is limited interface;

   type Input_Text_Stream_Access is access all Input_Text_Stream'Class;

   procedure Get
     (Self    : in out Input_Text_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean) is abstract;

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

end VSS.Text_Streams;
