--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Sax.Locators;

with VSS.Strings.Texts;
with VSS.XML.Locators;

package VSS.XML.Implementation.XmlAda_Locators is

   type XmlAda_Locator is
    new VSS.XML.Locators.SAX_Locator with record
      Locator : Sax.Locators.Locator;
   end record;

   overriding function Get_Column_Number
     (Self : XmlAda_Locator) return VSS.Strings.Character_Index'Base;

   overriding function Get_Line_Number
     (Self : XmlAda_Locator)
      return VSS.Strings.Texts.Line_Index'Base;

   overriding function Get_Public_Id
     (Self : XmlAda_Locator) return VSS.Strings.Virtual_String;

   overriding function Get_System_Id
     (Self : XmlAda_Locator) return VSS.Strings.Virtual_String;

end VSS.XML.Implementation.XmlAda_Locators;
