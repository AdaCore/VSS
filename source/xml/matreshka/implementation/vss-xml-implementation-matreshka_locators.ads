--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with XML.SAX.Locators;

with VSS.Strings;
with VSS.XML.Locators;

package VSS.XML.Implementation.Matreshka_Locators is

   type Matreshka_Locator is
    new VSS.XML.Locators.SAX_Locator with record
      Locator : Standard.XML.SAX.Locators.SAX_Locator;
   end record;

   overriding function Get_Column_Number
     (Self : Matreshka_Locator) return VSS.Strings.Character_Index'Base;

   overriding function Get_Line_Number
     (Self : Matreshka_Locator) return VSS.Strings.Line_Index'Base;

   overriding function Get_Public_Id
     (Self : Matreshka_Locator) return VSS.Strings.Virtual_String;

   overriding function Get_System_Id
     (Self : Matreshka_Locator) return VSS.Strings.Virtual_String;

end VSS.XML.Implementation.Matreshka_Locators;
