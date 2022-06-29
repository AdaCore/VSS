--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.XML.Lexical_Handlers is

   pragma Preelaborate;

   type SAX_Lexical_Handler is limited interface;

   type SAX_Lexical_Handler_Access is access all SAX_Lexical_Handler'Class;

   procedure Comment
     (Self    : in out SAX_Lexical_Handler;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

   procedure Start_DTD
     (Self      : in out SAX_Lexical_Handler;
      Name      : VSS.Strings.Virtual_String;
      Public_Id : VSS.Strings.Virtual_String;
      System_Id : VSS.Strings.Virtual_String;
      Success   : in out Boolean) is null;

   procedure End_DTD
     (Self    : in out SAX_Lexical_Handler;
      Success : in out Boolean) is null;

   procedure Start_CDATA
     (Self    : in out SAX_Lexical_Handler;
      Success : in out Boolean) is null;

   procedure End_CDATA
     (Self    : in out SAX_Lexical_Handler;
      Success : in out Boolean) is null;

   procedure Start_Entity
     (Self    : in out SAX_Lexical_Handler;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

   procedure End_Entity
     (Self    : in out SAX_Lexical_Handler;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

end VSS.XML.Lexical_Handlers;
