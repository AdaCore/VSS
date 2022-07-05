--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Vectors;

private with VSS.IRIs;
private with VSS.Strings;
with VSS.String_Vectors;
private with VSS.XML.Attributes;
with VSS.XML.Content_Handlers;
with VSS.XML.Error_Handlers;
private with VSS.XML.Attributes.Containers;
private with VSS.XML.Implementation.Error_Handlers;
limited private with VSS.XML.Implementation.Template_Namespaces;
private with VSS.XML.Implementation.Template_Parsers;
--  with VSS.XML.Lexical_Handlers;
private with VSS.XML.Locators;
limited with VSS.XML.Templates.Proxies;

package VSS.XML.Templates is

   type Proxy_Access is
     access all VSS.XML.Templates.Proxies.Abstract_Proxy'Class;

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
      Proxy : not null Proxy_Access)
     with Pre => not Path.Is_Empty;
   --  Bind given proxy to given path. Ownership of the proxy object is moved
   --  to template processor, thus application must not use it anymore.
   --  Proxy will be released by the templates processor.

private

   type Event_Kind is (None, Start_Element, End_Element, Text);

   type Event_Record (Kind : Event_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Start_Element | End_Element =>
            URI            : VSS.IRIs.IRI;
            Local_Name     : VSS.Strings.Virtual_String;
            Qualified_Name : VSS.Strings.Virtual_String;

            case Kind is
               when Start_Element =>
                  Attributes : VSS.XML.Attributes.Containers.Attributes;

               when others =>
                  null;
            end case;

         when Text =>
            Text : VSS.Strings.Virtual_String;
      end case;
   end record;

   package Event_Vectors is
     new Ada.Containers.Vectors (Positive, Event_Record);

   type Named_Item is abstract tagged null record;

   --  function Evaluate
   --    (Self : Named_Item;
   --     Name : VSS.String_Vectors.Virtual_String_Vector)
   --     return Event_Vectors.Vector is abstract;

   type Iterable_Item is new Named_Item with record
      null;
   end record;

   --  function Evaluate
   --    (Self : Iterable_Item;
   --     Name : VSS.String_Vectors.Virtual_String_Vector)
   --     return Event_Vectors.Vector;

   type State_Record is record
      Depth     : Natural := 1;
      Recording : Boolean := False;
      Stream    : Event_Vectors.Vector;
   end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Positive, State_Record);

   type Namespace_Access is
     access all VSS.XML.Implementation.Template_Namespaces.Namespace'Class;

   type XML_Template_Processor is
     limited new VSS.XML.Content_Handlers.SAX_Content_Handler with record
      Locator : VSS.XML.Locators.SAX_Locator_Access;
      Content : VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
      Error   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;

      Binded  : Namespace_Access;

      Depth   : Natural := 0;
      Parser  : VSS.XML.Implementation.Template_Parsers.Template_Parser;

      --  Current : State_Record;
      --  Stack   : State_Vectors.Vector;
   end record;

   overriding procedure Set_Document_Locator
     (Self    : in out XML_Template_Processor;
      Locator : VSS.XML.Locators.SAX_Locator_Access;
      Success : in out Boolean);

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

end VSS.XML.Templates;
