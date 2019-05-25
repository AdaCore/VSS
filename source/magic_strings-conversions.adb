------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Magic_Strings.UTF8;

package body Magic_Strings.Conversions is

   ---------------------
   -- To_Magic_String --
   ---------------------

   function To_Magic_String
     (Item : Ada.Strings.UTF_Encoding.UTF_8_String) return Magic_String
   is
      Segment : String_Access;
      Success : Boolean;

   begin
      Magic_Strings.UTF8.From_UTF_8_String (Item, Segment, Success);

      if not Success then
         raise Constraint_Error with "Invalid UTF-8 string";
      end if;

      return (Ada.Finalization.Controlled with Data => Segment);
   end To_Magic_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Item : Magic_String) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return "";
   end To_UTF_8_String;

end Magic_Strings.Conversions;
