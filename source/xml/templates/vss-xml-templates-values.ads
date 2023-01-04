--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;
with VSS.XML.Event_Vectors;

package VSS.XML.Templates.Values is

   type Value_Kind is (Error, Nothing, Default, Boolean, String, Content);

   type Value (Kind : Value_Kind) is record
      case Kind is
         when Nothing | Default =>
            null;

         when Error =>
            Message       : VSS.Strings.Virtual_String;

         when Boolean =>
            Boolean_Value : Standard.Boolean;

         when String =>
            String_Value  : VSS.Strings.Virtual_String;

         when Content =>
            Content_Value : VSS.XML.Event_Vectors.Vector;
      end case;
   end record;

end VSS.XML.Templates.Values;
