--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Sax.Readers;

with VSS.IRIs;
with VSS.Strings;
with VSS.XML.Attributes;

package VSS.XML.Implementation.XmlAda_Attributes is

   type XmlAda_Attributes is
     limited new VSS.XML.Attributes.XML_Attributes with record
      Attributes : Sax.Readers.Sax_Attribute_List;
   end record;

   overriding function Get_Length (Self : XmlAda_Attributes) return Natural;

   overriding function Get_URI
     (Self  : XmlAda_Attributes;
      Index : Positive) return VSS.IRIs.IRI;

   overriding function Get_Name
     (Self  : XmlAda_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String;

   overriding function Get_Value
     (Self  : XmlAda_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String;

end VSS.XML.Implementation.XmlAda_Attributes;
