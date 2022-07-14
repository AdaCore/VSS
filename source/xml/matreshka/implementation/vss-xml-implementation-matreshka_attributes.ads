--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with XML.SAX.Attributes;

with VSS.IRIs;
with VSS.Strings;
with VSS.XML.Attributes;

package VSS.XML.Implementation.Matreshka_Attributes is

   type Matreshka_Attributes is
     limited new VSS.XML.Attributes.XML_Attributes with record
      Attributes : Standard.XML.SAX.Attributes.SAX_Attributes;
   end record;

   overriding function Get_Length
     (Self : Matreshka_Attributes) return Natural;

   overriding function Get_URI
     (Self  : Matreshka_Attributes;
      Index : Positive) return VSS.IRIs.IRI;

   overriding function Get_Name
     (Self  : Matreshka_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String;

   overriding function Get_Value
     (Self  : Matreshka_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String;

end VSS.XML.Implementation.Matreshka_Attributes;
