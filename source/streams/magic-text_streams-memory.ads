------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Magic.Stream_Element_Buffers;

package Magic.Text_Streams.Memory is

   type Memory_UTF8_Output_Stream is
   limited new Magic.Text_Streams.Output_Text_Stream with record
      Buffer : Magic.Stream_Element_Buffers.Stream_Element_Buffer;
   end record;

   procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : Magic.Characters.Magic_Character;
      Success : in out Boolean);

end Magic.Text_Streams.Memory;
