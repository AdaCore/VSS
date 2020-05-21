------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Magic.Characters;
with Magic.Strings;

package Magic.Text_Streams is

   pragma Preelaborate;

   type Input_Text_Stream is limited interface;

   type Input_Text_Stream_Access is access all Input_Text_Stream'Class;

   procedure Get
     (Self    : in out Input_Text_Stream;
      Item    : out Magic.Characters.Magic_Character;
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
     (Self : Input_Text_Stream) return Magic.Strings.Magic_String is abstract;
   --  Return error message when Has_Error returns True, or 'null' string.

   type Output_Text_Stream is limited interface;

   type Output_Text_Stream_Access is access all Output_Text_Stream'Class;

   procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : Magic.Characters.Magic_Character;
      Success : in out Boolean) is abstract;

end Magic.Text_Streams;
