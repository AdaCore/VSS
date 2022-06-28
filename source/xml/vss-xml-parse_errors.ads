--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Texts;

package VSS.XML.Parse_Errors is

   pragma Preelaborate;

   type SAX_Parse_Error is limited interface;

   function Get_Message
     (Self : SAX_Parse_Error) return VSS.Strings.Virtual_String is abstract;

   --  function Get_Exception
   --    (Self : SAX_Parse_Error) return Ada.Exceptions.Exception_Occurrence;

   function Get_Public_Id
     (Self : SAX_Parse_Error) return VSS.Strings.Virtual_String is abstract;

   function Get_System_Id
     (Self : SAX_Parse_Error) return VSS.Strings.Virtual_String is abstract;

   function Get_Line_Number
     (Self : SAX_Parse_Error)
      return VSS.Strings.Texts.Line_Index'Base is abstract;

   function Get_Column_Number
     (Self : SAX_Parse_Error)
      return VSS.Strings.Character_Index'Base is abstract;

end VSS.XML.Parse_Errors;
