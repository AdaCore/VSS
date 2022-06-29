--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Sax.Exceptions;

with VSS.Strings.Texts;
with VSS.XML.Parse_Errors;

package VSS.XML.Implementation.XmlAda_Parse_Errors is

   type Parse_Error is
     limited new VSS.XML.Parse_Errors.SAX_Parse_Error with
   record
      Error :
        not null access constant Sax.Exceptions.Sax_Parse_Exception'Class;
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

end VSS.XML.Implementation.XmlAda_Parse_Errors;
