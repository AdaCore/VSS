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

with Magic.Characters;

package Magic.Text_Streams is

   pragma Preelaborate;

   type Input_Text_Stream is limited interface;

   type Input_Text_Stream_Access is access all Input_Text_Stream'Class;

   procedure Get
     (Self    : in out Input_Text_Stream;
      Item    : out Magic.Characters.Magic_Character;
      Success : in out Boolean) is abstract;

   type Output_Text_Stream is limited interface;

   type Output_Text_Stream_Access is access all Output_Text_Stream'Class;

   procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : Magic.Characters.Magic_Character;
      Success : in out Boolean) is abstract;

end Magic.Text_Streams;
