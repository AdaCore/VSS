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

private with Ada.Streams;

with VSS.Stream_Element_Buffers;

package VSS.Text_Streams.Memory is

   type Memory_UTF8_Input_Stream is
     limited new VSS.Text_Streams.Input_Text_Stream with private;

   ------------------------------
   -- Memory_UTF8_Input_Stream --
   ------------------------------

   type Memory_UTF8_Output_Stream is
   limited new VSS.Text_Streams.Output_Text_Stream with record
      Buffer : VSS.Stream_Element_Buffers.Stream_Element_Buffer;
   end record;

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

private

   type Memory_UTF8_Input_Stream is
     limited new VSS.Text_Streams.Input_Text_Stream with record
      Buffer    : VSS.Stream_Element_Buffers.Stream_Element_Buffer;
      Current   : Ada.Streams.Stream_Element_Count := 1;
      Diagnosis : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Get
     (Self    : in out Memory_UTF8_Input_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding function Is_End_Of_Data
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Is_End_Of_Stream
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Has_Error
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Error_Message
     (Self : Memory_UTF8_Input_Stream) return VSS.Strings.Virtual_String;

end VSS.Text_Streams.Memory;
