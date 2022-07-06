--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Texts;
with VSS.XML.Locators;
with VSS.XML.Parse_Errors;

package VSS.XML.Implementation.Parse_Errors is

   type Parse_Error is
     limited new VSS.XML.Parse_Errors.SAX_Parse_Error with
   record
      Locator : VSS.XML.Locators.SAX_Locator_Access;
      Message : VSS.Strings.Virtual_String;
   end record;

   overriding function Get_Column_Number
     (Self : Parse_Error) return VSS.Strings.Character_Index'Base;

   overriding function Get_Line_Number
     (Self : Parse_Error) return VSS.Strings.Texts.Line_Index'Base;

   overriding function Get_Message
     (Self : Parse_Error) return VSS.Strings.Virtual_String;

   overriding function Get_Public_Id
     (Self : Parse_Error) return VSS.Strings.Virtual_String;

   overriding function Get_System_Id
     (Self : Parse_Error) return VSS.Strings.Virtual_String;

   type Parse_Error_Location is
     limited new VSS.XML.Parse_Errors.SAX_Parse_Error with
   record
      Public_Id : VSS.Strings.Virtual_String;
      System_Id : VSS.Strings.Virtual_String;
      Line      : VSS.Strings.Texts.Line_Count := 0;
      Column    : VSS.Strings.Character_Count  := 0;
      Message   : VSS.Strings.Virtual_String;
   end record;

   overriding function Get_Column_Number
     (Self : Parse_Error_Location) return VSS.Strings.Character_Index'Base;

   overriding function Get_Line_Number
     (Self : Parse_Error_Location) return VSS.Strings.Texts.Line_Index'Base;

   overriding function Get_Message
     (Self : Parse_Error_Location) return VSS.Strings.Virtual_String;

   overriding function Get_Public_Id
     (Self : Parse_Error_Location) return VSS.Strings.Virtual_String;

   overriding function Get_System_Id
     (Self : Parse_Error_Location) return VSS.Strings.Virtual_String;

end VSS.XML.Implementation.Parse_Errors;
