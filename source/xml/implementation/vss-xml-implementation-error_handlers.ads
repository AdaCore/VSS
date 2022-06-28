--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.XML.Error_Handlers;
with VSS.XML.Parse_Errors;

package VSS.XML.Implementation.Error_Handlers is

   type Default_Error_Handler is
     limited new VSS.XML.Error_Handlers.SAX_Error_Handler with null record;

   overriding procedure Warning
     (Self    : in out Default_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean);

   overriding procedure Error
     (Self    : in out Default_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean);

   overriding procedure Fatal_Error
     (Self    : in out Default_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean);

   Default : aliased Default_Error_Handler;

end VSS.XML.Implementation.Error_Handlers;
