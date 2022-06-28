--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Texts;

package VSS.XML.Locators is

   pragma Preelaborate;

   type SAX_Locator is limited interface;

   type SAX_Locator_Access is access all SAX_Locator'Class;

   function Get_Column_Number
     (Self : SAX_Locator) return VSS.Strings.Character_Index'Base is abstract;

   function Get_Line_Number
     (Self : SAX_Locator)
      return VSS.Strings.Texts.Line_Index'Base is abstract;

   function Get_Public_Id
     (Self : SAX_Locator) return VSS.Strings.Virtual_String is abstract;

   function Get_System_Id
     (Self : SAX_Locator) return VSS.Strings.Virtual_String is abstract;

end VSS.XML.Locators;
