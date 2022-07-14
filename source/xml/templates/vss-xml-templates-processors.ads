--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with VSS.IRIs;
private with VSS.Strings;
with VSS.String_Vectors;
private with VSS.XML.Attributes;
with VSS.XML.Content_Handlers;
with VSS.XML.Error_Handlers;
private with VSS.XML.Implementation.Error_Handlers;
private with VSS.XML.Implementation.Template_Namespaces;
private with VSS.XML.Implementation.Template_Parsers;
--  with VSS.XML.Lexical_Handlers;
private with VSS.XML.Locators;
with VSS.XML.Templates.Proxies;

package VSS.XML.Templates.Processors is

   type XML_Template_Processor is
     limited new VSS.XML.Content_Handlers.SAX_Content_Handler with private;

   procedure Set_Content_Handler
     (Self    : in out XML_Template_Processor;
      Handler : VSS.XML.Content_Handlers.SAX_Content_Handler_Access);

   --  procedure Set_Lexical_Handler
   --    (Self    : in out SAX_Reader;
   --     Handler : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access)
   --       is abstract;

   procedure Set_Error_Handler
     (Self    : in out XML_Template_Processor;
      Handler : VSS.XML.Error_Handlers.SAX_Error_Handler_Access);

   procedure Bind
     (Self  : in out XML_Template_Processor'Class;
      Path  : VSS.String_Vectors.Virtual_String_Vector;
      Proxy : not null VSS.XML.Templates.Proxies.Proxy_Access)
     with Pre => not Path.Is_Empty;
   --  Bind given proxy to given path. Ownership of the proxy object is moved
   --  to template processor, thus application must not use it anymore.
   --  Proxy will be released by the templates processor.

private

   type XML_Template_Processor is
     limited new VSS.XML.Content_Handlers.SAX_Content_Handler with
   record
      Locator : VSS.XML.Locators.SAX_Locator_Access;
      Content : VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
      Error   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;

      Binded  : aliased VSS.XML.Implementation.Template_Namespaces.Namespace;

      Depth   : Natural := 0;
      Parser  : VSS.XML.Implementation.Template_Parsers.Template_Parser;
   end record;

   overriding procedure Set_Document_Locator
     (Self    : in out XML_Template_Processor;
      Locator : VSS.XML.Locators.SAX_Locator_Access);

   overriding procedure Start_Document
     (Self    : in out XML_Template_Processor;
      Success : in out Boolean);

   overriding procedure End_Document
     (Self    : in out XML_Template_Processor;
      Success : in out Boolean);

   --  procedure Start_Prefix_Mapping
   --    (Self    : in out SAX_Content_Handler;
   --     Prefix  : VSS.Strings.Virtual_String;
   --     URI     : VSS.IRIs.IRI;
   --     Success : in out Boolean) is null;
   --
   --  procedure End_Prefix_Mapping
   --    (Self    : in out SAX_Content_Handler;
   --     Prefix  : VSS.Strings.Virtual_String;
   --     Success : in out Boolean) is null;

   overriding procedure Start_Element
     (Self       : in out XML_Template_Processor;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean);

   overriding procedure End_Element
     (Self    : in out XML_Template_Processor;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Characters
     (Self    : in out XML_Template_Processor;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Ignorable_Whitespace
     (Self    : in out XML_Template_Processor;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) renames Characters;

   overriding procedure Processing_Instruction
     (Self    : in out XML_Template_Processor;
      Target  : VSS.Strings.Virtual_String;
      Data    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   --  procedure Skipped_Entity
   --    (Self    : in out SAX_Content_Handler;
   --     Name    : VSS.Strings.Virtual_String;
   --     Success : in out Boolean) is null;

end VSS.XML.Templates.Processors;
