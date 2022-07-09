--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.XML.Templates.Values is

   type Value_Kind is (Error, Nothing, Default, Boolean, String);

   type Value (Kind : Value_Kind) is record
      case Kind is
         when Nothing | Default =>
            null;

         when Error =>
            Message : VSS.Strings.Virtual_String;

         when Boolean =>
            Boolean_Value : Standard.Boolean;

         when String =>
            String_Value  : VSS.Strings.Virtual_String;
      end case;
   end record;

end VSS.XML.Templates.Values;
