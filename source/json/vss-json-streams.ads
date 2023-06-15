--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Strings;

package VSS.JSON.Streams is

   pragma Preelaborate;

   type JSON_Stream_Element_Kind is
     (None,
      Invalid,
      Start_Document,
      End_Document,
      Comment,
      --  Comment in JSON5 format, not implemented.
      Start_Array,
      End_Array,
      Start_Object,
      End_Object,
      Key_Name,
      String_Value,
      Number_Value,
      Boolean_Value,
      Null_Value);
   --  Kinds of elements in the JSON stream.

   type JSON_Stream_Element (Kind : JSON_Stream_Element_Kind := None) is record
      case Kind is
         when None | Invalid | Start_Document | End_Document =>
            null;

         when Comment =>
            Text : VSS.Strings.Virtual_String;

         when Start_Array | End_Array | Start_Object | End_Object =>
            null;

         when Key_Name =>
            Key_Name : VSS.Strings.Virtual_String;

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

end VSS.JSON.Streams;
