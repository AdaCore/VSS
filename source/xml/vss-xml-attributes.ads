--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.IRIs;
with VSS.Strings;

package VSS.XML.Attributes is

   pragma Preelaborate;

   type XML_Attributes is limited interface;

   function Get_Length (Self : XML_Attributes) return Natural is abstract;

   function Get_URI
     (Self  : XML_Attributes;
      Index : Positive) return VSS.IRIs.IRI is abstract;

   function Get_Name
     (Self  : XML_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is abstract;

   function Get_Value
     (Self  : XML_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is abstract;

end VSS.XML.Attributes;
