--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

private with VSS.IRIs;
private with VSS.Strings;
private with VSS.XML.Attributes;
private with VSS.XML.Implementation.Error_Handlers;
private with VSS.XML.Locators;
private with VSS.XML.Namespace_Maps;

package VSS.XML.Writers.Simple is

   type Attribute_Syntax is (Auto, Single_Quoted, Double_Quoted);
   --  Syntax used for attribute value of the element.

   type Simple_XML_Writer is
     limited new VSS.XML.Writers.XML_Writer with private;

   procedure Set_Attribute_Syntax
     (Self   : in out Simple_XML_Writer'Class;
      Syntax : Attribute_Syntax);

private

   type Writer_Settings is limited record
      Syntax : Attribute_Syntax := Auto;
   end record;

   type Simple_XML_Writer is
     limited new VSS.XML.Writers.XML_Writer with record
      Settings       : Writer_Settings;

      Output         : VSS.Text_Streams.Output_Text_Stream_Access;
      Error          : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;
      Locator        : VSS.XML.Locators.SAX_Locator_Access;
      Namespace_Map  : VSS.XML.Namespace_Maps.XML_Namespace_Map;

      Syntax         : Attribute_Syntax;
      CDATA_Mode     : Boolean;
      Start_Tag_Open : Boolean;
   end record;

   overriding procedure Set_Document_Locator
     (Self    : in out Simple_XML_Writer;
      Locator : VSS.XML.Locators.SAX_Locator_Access);

   overriding procedure Start_Document
     (Self    : in out Simple_XML_Writer;
      Success : in out Boolean);

   overriding procedure End_Document
     (Self    : in out Simple_XML_Writer;
      Success : in out Boolean);

   overriding procedure Start_Prefix_Mapping
     (Self    : in out Simple_XML_Writer;
      Prefix  : VSS.Strings.Virtual_String;
      URI     : VSS.IRIs.IRI;
      Success : in out Boolean);

   overriding procedure End_Prefix_Mapping
     (Self    : in out Simple_XML_Writer;
      Prefix  : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Start_Element
     (Self       : in out Simple_XML_Writer;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean);

   overriding procedure End_Element
     (Self    : in out Simple_XML_Writer;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Characters
     (Self    : in out Simple_XML_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Ignorable_Whitespace
     (Self    : in out Simple_XML_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) renames Characters;

   overriding procedure Processing_Instruction
     (Self    : in out Simple_XML_Writer;
      Target  : VSS.Strings.Virtual_String;
      Data    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   --  procedure Skipped_Entity
   --    (Self    : in out SAX_Content_Handler;
   --     Name    : VSS.Strings.Virtual_String;
   --     Success : in out Boolean) is null;

   overriding procedure Comment
     (Self    : in out Simple_XML_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   --  procedure Start_DTD
   --    (Self      : in out SAX_Lexical_Handler;
   --     Name      : VSS.Strings.Virtual_String;
   --     Public_Id : VSS.Strings.Virtual_String;
   --     System_Id : VSS.Strings.Virtual_String;
   --     Success   : in out Boolean) is null;
   --
   --  procedure End_DTD
   --    (Self    : in out SAX_Lexical_Handler;
   --     Success : in out Boolean) is null;

   overriding procedure Start_CDATA
     (Self    : in out Simple_XML_Writer;
      Success : in out Boolean);

   overriding procedure End_CDATA
     (Self    : in out Simple_XML_Writer;
      Success : in out Boolean);

   --  procedure Start_Entity
   --    (Self    : in out SAX_Lexical_Handler;
   --     Name    : VSS.Strings.Virtual_String;
   --     Success : in out Boolean) is null;
   --
   --  procedure End_Entity
   --    (Self    : in out SAX_Lexical_Handler;
   --     Name    : VSS.Strings.Virtual_String;
   --     Success : in out Boolean) is null;

   overriding procedure Set_Output_Stream
     (Self   : in out Simple_XML_Writer;
      Stream : VSS.Text_Streams.Output_Text_Stream_Access);

   overriding procedure Set_Error_Handler
     (Self : in out Simple_XML_Writer;
      To   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access);

end VSS.XML.Writers.Simple;
