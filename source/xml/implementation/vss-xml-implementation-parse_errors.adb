--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Implementation.Parse_Errors is

   use type VSS.XML.Locators.SAX_Locator_Access;

   -----------------------
   -- Get_Column_Number --
   -----------------------

   overriding function Get_Column_Number
     (Self : Parse_Error) return VSS.Strings.Character_Index'Base is
   begin
      return
        (if Self.Locator /= null then Self.Locator.Get_Column_Number else 0);
   end Get_Column_Number;

   -----------------------
   -- Get_Column_Number --
   -----------------------

   overriding function Get_Column_Number
     (Self : Parse_Error_Location) return VSS.Strings.Character_Index'Base is
   begin
      return Self.Column;
   end Get_Column_Number;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   overriding function Get_Line_Number
     (Self : Parse_Error) return VSS.Strings.Texts.Line_Index'Base is
   begin
      return
        (if Self.Locator /= null then Self.Locator.Get_Line_Number else 0);
   end Get_Line_Number;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   overriding function Get_Line_Number
     (Self : Parse_Error_Location) return VSS.Strings.Texts.Line_Index'Base is
   begin
      return Self.Line;
   end Get_Line_Number;

   -----------------
   -- Get_Message --
   -----------------

   overriding function Get_Message
     (Self : Parse_Error) return VSS.Strings.Virtual_String is
   begin
      return Self.Message;
   end Get_Message;

   -----------------
   -- Get_Message --
   -----------------

   overriding function Get_Message
     (Self : Parse_Error_Location) return VSS.Strings.Virtual_String is
   begin
      return Self.Message;
   end Get_Message;

   -------------------
   -- Get_Public_Id --
   -------------------

   overriding function Get_Public_Id
     (Self : Parse_Error) return VSS.Strings.Virtual_String is
   begin
      return
        (if Self.Locator /= null
         then Self.Locator.Get_Public_Id
         else VSS.Strings.Empty_Virtual_String);
   end Get_Public_Id;

   -------------------
   -- Get_Public_Id --
   -------------------

   overriding function Get_Public_Id
     (Self : Parse_Error_Location) return VSS.Strings.Virtual_String is
   begin
      return Self.Public_Id;
   end Get_Public_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   overriding function Get_System_Id
     (Self : Parse_Error) return VSS.Strings.Virtual_String is
   begin
      return
        (if Self.Locator /= null
         then Self.Locator.Get_System_Id
         else VSS.Strings.Empty_Virtual_String);
   end Get_System_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   overriding function Get_System_Id
     (Self : Parse_Error_Location) return VSS.Strings.Virtual_String is
   begin
      return Self.System_Id;
   end Get_System_Id;

end VSS.XML.Implementation.Parse_Errors;
