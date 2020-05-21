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

private with Ada.Finalization;
with Ada.Streams;

package Magic.Stream_Element_Buffers is

   pragma Preelaborate;
   pragma Remote_Types;

   type Stream_Element_Buffer is tagged private;

   procedure Set_Capacity
     (Self     : in out Stream_Element_Buffer'Class;
      Capacity : Ada.Streams.Stream_Element_Count);
   --  Request to preallocate memory to store given number of stream elements.

   function Length
     (Self : Stream_Element_Buffer'Class)
      return Ada.Streams.Stream_Element_Count;
   --  Return size of accumulated data.

   function Element
     (Self  : Stream_Element_Buffer'Class;
      Index : Ada.Streams.Stream_Element_Count)
      return Ada.Streams.Stream_Element;
   --  Return element at given index.

   procedure Append
     (Self : in out Stream_Element_Buffer;
      Item : Ada.Streams.Stream_Element);
   --  Append stream element to the end of the buffer

private

   type Data_Record (Size : Ada.Streams.Stream_Element_Count) is record
      Length  : Ada.Streams.Stream_Element_Count;
      Storage : Ada.Streams.Stream_Element_Array (1 .. Size);
   end record;

   type Data_Access is access all Data_Record;

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Stream_Element_Buffer);

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Stream_Element_Buffer);

   --  function Input
   --    (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   --     return Stream_Element_Buffer;
   --
   --  procedure Output
   --    (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
   --     Item   : Stream_Element_Buffer);

   type Stream_Element_Buffer is new Ada.Finalization.Controlled with record
      Data     : Data_Access;
      Capacity : Ada.Streams.Stream_Element_Count := 0;
   end record;

   for Stream_Element_Buffer'Read use Read;
   for Stream_Element_Buffer'Write use Write;
   --  for Stream_Element_Buffer'Input use Input;
   --  for Stream_Element_Buffer'Output use Output;

   overriding procedure Adjust (Self : in out Stream_Element_Buffer);

   overriding procedure Finalize (Self : in out Stream_Element_Buffer);

end Magic.Stream_Element_Buffers;
