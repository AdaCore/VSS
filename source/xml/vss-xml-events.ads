--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.IRIs;
with VSS.Strings;

package VSS.XML.Events is

   pragma Preelaborate;

   type XML_Event_Kind is
     (None,
      Start_Element,
      Attribute,
      End_Element,
      Comment,
      Processing_Instruction,
      Text,
      CDATA);

   type XML_Event (Kind : XML_Event_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Start_Element | Attribute | End_Element =>
            URI  : VSS.IRIs.IRI;
            Name : VSS.Strings.Virtual_String;

            case Kind is
               when Attribute =>
                  Value : VSS.Strings.Virtual_String;

               when others =>
                  null;
            end case;

         when Text | CDATA | Comment =>
            Text : VSS.Strings.Virtual_String;

         when Processing_Instruction =>
            Target : VSS.Strings.Virtual_String;
            Data   : VSS.Strings.Virtual_String;
      end case;
   end record;

end VSS.XML.Events;
