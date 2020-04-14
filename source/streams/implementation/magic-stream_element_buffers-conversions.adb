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

package body Magic.Stream_Element_Buffers.Conversions is

   -------------------------
   -- Unchecked_To_String --
   -------------------------

   function Unchecked_To_String
     (Item : Stream_Element_Buffer'Class) return String
   is
      use type Ada.Streams.Stream_Element_Offset;

   begin
      if Item.Data = null or else Item.Data.Length = 0 then
         return "";
      end if;

      declare
         Result : String (1 .. Natural (Item.Data.Length))
           with Address => Item.Data.Storage (Item.Data.Storage'First)'Address;

      begin
         return Result;
      end;
   end Unchecked_To_String;

end Magic.Stream_Element_Buffers.Conversions;
