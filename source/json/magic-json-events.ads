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

with Magic.Strings;

package Magic.JSON.Events is

   pragma Preelaborate;

   type JSON_Event_Kind is
     (None,
      Start_Array,
      End_Array,
      Start_Object,
      End_Object,
      Key_Name,
      String_Value,
      Number_Value,
      Boolean_Value,
      Null_Value);

   type JSON_Event (Kind : JSON_Event_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Start_Array | End_Array | Start_Object | End_Object =>
            null;

         when Key_Name =>
            Key : Magic.Strings.Magic_String;

         when String_Value =>
            String_Value : Magic.Strings.Magic_String;

         when Number_Value =>
            Number_Value : Magic.JSON.JSON_Number;

         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Null_Value =>
            null;
      end case;
   end record;

end Magic.JSON.Events;
