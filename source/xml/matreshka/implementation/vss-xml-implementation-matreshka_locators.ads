--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with XML.SAX.Locators;

with VSS.Strings.Texts;
with VSS.XML.Locators;

package VSS.XML.Implementation.Matreshka_Locators is

   type Matreshka_Locator is
    new VSS.XML.Locators.SAX_Locator with record
      Locator : Standard.XML.SAX.Locators.SAX_Locator;
   end record;

   overriding function Get_Column_Number
     (Self : Matreshka_Locator) return VSS.Strings.Character_Index'Base;

   overriding function Get_Line_Number
     (Self : Matreshka_Locator)
      return VSS.Strings.Texts.Line_Index'Base;

   overriding function Get_Public_Id
     (Self : Matreshka_Locator) return VSS.Strings.Virtual_String;

   overriding function Get_System_Id
     (Self : Matreshka_Locator) return VSS.Strings.Virtual_String;

end VSS.XML.Implementation.Matreshka_Locators;
