--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Sax.Symbols;

with VSS.Strings.Conversions;

package body VSS.XML.Implementation.XmlAda_Locators is

   -----------------------
   -- Get_Column_Number --
   -----------------------

   overriding function Get_Column_Number
     (Self : XmlAda_Locator) return VSS.Strings.Character_Index'Base is
   begin
      return
        VSS.Strings.Character_Index'Base
          (Sax.Locators.Get_Column_Number (Self.Locator));
   end Get_Column_Number;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   overriding function Get_Line_Number
     (Self : XmlAda_Locator)
      return VSS.Strings.Texts.Line_Index'Base is
   begin
      return
        VSS.Strings.Texts.Line_Index'Base
          (Sax.Locators.Get_Line_Number (Self.Locator));
   end Get_Line_Number;

   -------------------
   -- Get_Public_Id --
   -------------------

   overriding function Get_Public_Id
     (Self : XmlAda_Locator) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Sax.Symbols.Get (Sax.Locators.Get_Public_Id (Self.Locator)).all);
   end Get_Public_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   overriding function Get_System_Id
     (Self : XmlAda_Locator) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Sax.Symbols.Get (Sax.Locators.Get_System_Id (Self.Locator)).all);
   end Get_System_Id;

end VSS.XML.Implementation.XmlAda_Locators;
