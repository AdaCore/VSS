--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Sax.Symbols;

with VSS.Strings.Conversions;

package body VSS.XML.Implementation.XmlAda_Parse_Errors is

   -----------------------
   -- Get_Column_Number --
   -----------------------

   overriding function Get_Column_Number
     (Self : Parse_Error) return VSS.Strings.Character_Index'Base is
   begin
      return
        VSS.Strings.Character_Index'Base
          (Sax.Exceptions.Get_Location (Self.Error.all).Column);
   end Get_Column_Number;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   overriding function Get_Line_Number
     (Self : Parse_Error) return VSS.Strings.Texts.Line_Index'Base is
   begin
      return
        VSS.Strings.Texts.Line_Index'Base
          (Sax.Exceptions.Get_Location (Self.Error.all).Line);
   end Get_Line_Number;

   -----------------
   -- Get_Message --
   -----------------

   overriding function Get_Message
     (Self : Parse_Error) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Sax.Exceptions.Get_Message (Self.Error.all));
   end Get_Message;

   -------------------
   -- Get_Public_Id --
   -------------------

   overriding function Get_Public_Id
     (Self : Parse_Error) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Sax.Symbols.Get
             (Sax.Exceptions.Get_Location (Self.Error.all).Public_Id).all);
   end Get_Public_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   overriding function Get_System_Id
     (Self : Parse_Error) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Sax.Symbols.Get
             (Sax.Exceptions.Get_Location (Self.Error.all).System_Id).all);
   end Get_System_Id;

end VSS.XML.Implementation.XmlAda_Parse_Errors;
