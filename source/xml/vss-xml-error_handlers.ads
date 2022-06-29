--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.XML.Parse_Errors;

package VSS.XML.Error_Handlers is

   pragma Preelaborate;

   type SAX_Error_Handler is limited interface;

   type SAX_Error_Handler_Access is access all SAX_Error_Handler'Class;

   procedure Warning
     (Self    : in out SAX_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean) is null;

   procedure Error
     (Self    : in out SAX_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean) is null;

   procedure Fatal_Error
     (Self    : in out SAX_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean) is null;

end VSS.XML.Error_Handlers;
