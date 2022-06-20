--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.JSON.Events is

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
            Key : VSS.Strings.Virtual_String;

         when String_Value =>
            String_Value : VSS.Strings.Virtual_String;

         when Number_Value =>
            Number_Value : VSS.JSON.JSON_Number;

         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Null_Value =>
            null;
      end case;
   end record;

end VSS.JSON.Events;
