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

with Magic.Strings.UTF8;

package body Magic.Strings.Conversions is

   ---------------------
   -- To_Magic_String --
   ---------------------

   function To_Magic_String
     (Item : Ada.Strings.UTF_Encoding.UTF_8_String) return Magic_String
   is
      Segment : String_Access;
      Success : Boolean;

   begin
      Magic.Strings.UTF8.From_UTF_8_String (Item, Segment, Success);

      if not Success then
         raise Constraint_Error with "Ill-formed UTF-8 data";
      end if;

      return (Ada.Finalization.Controlled with Data => Segment, others => <>);
   end To_Magic_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Item : Magic_String) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      if Item.Data = null then
         return "";

      else
         return Item.Data.To_UTF_8_String;
      end if;
   end To_UTF_8_String;

end Magic.Strings.Conversions;
