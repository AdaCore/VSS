--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Implementation.Matreshka_Locators is

   -----------------------
   -- Get_Column_Number --
   -----------------------

   overriding function Get_Column_Number
     (Self : Matreshka_Locator) return VSS.Strings.Character_Index'Base is
   begin
      return VSS.Strings.Character_Index'Base (Self.Locator.Column);
   end Get_Column_Number;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   overriding function Get_Line_Number
     (Self : Matreshka_Locator)
      return VSS.Strings.Texts.Line_Index'Base is
   begin
      return VSS.Strings.Texts.Line_Index'Base (Self.Locator.Line);
   end Get_Line_Number;

   -------------------
   -- Get_Public_Id --
   -------------------

   overriding function Get_Public_Id
     (Self : Matreshka_Locator) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.To_Virtual_String
          (Self.Locator.Public_Id.To_Wide_Wide_String);
   end Get_Public_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   overriding function Get_System_Id
     (Self : Matreshka_Locator) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.To_Virtual_String
          (Self.Locator.System_Id.To_Wide_Wide_String);
   end Get_System_Id;

end VSS.XML.Implementation.Matreshka_Locators;
