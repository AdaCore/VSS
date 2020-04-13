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
