--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.IRIs;
with VSS.Strings;

with VSS.XML.Attributes;
with VSS.XML.Locators;

package VSS.XML.Content_Handlers is

   pragma Preelaborate;

   type SAX_Content_Handler is limited interface;

   type SAX_Content_Handler_Access is access all SAX_Content_Handler'Class;

   procedure Set_Document_Locator
     (Self    : in out SAX_Content_Handler;
      Locator : VSS.XML.Locators.SAX_Locator_Access) is null;

   procedure Start_Document
     (Self    : in out SAX_Content_Handler;
      Success : in out Boolean) is null;

   procedure End_Document
     (Self    : in out SAX_Content_Handler;
      Success : in out Boolean) is null;

   procedure Start_Prefix_Mapping
     (Self    : in out SAX_Content_Handler;
      Prefix  : VSS.Strings.Virtual_String;
      URI     : VSS.IRIs.IRI;
      Success : in out Boolean) is null;

   procedure End_Prefix_Mapping
     (Self    : in out SAX_Content_Handler;
      Prefix  : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

   procedure Start_Element
     (Self       : in out SAX_Content_Handler;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean) is null;

   procedure End_Element
     (Self    : in out SAX_Content_Handler;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

   procedure Characters
     (Self    : in out SAX_Content_Handler;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

   procedure Ignorable_Whitespace
     (Self    : in out SAX_Content_Handler;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

   procedure Processing_Instruction
     (Self    : in out SAX_Content_Handler;
      Target  : VSS.Strings.Virtual_String;
      Data    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

   procedure Skipped_Entity
     (Self    : in out SAX_Content_Handler;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is null;

end VSS.XML.Content_Handlers;
