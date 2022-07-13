--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Implementation of SAX_Reader on top of Matreshka's Simple_Reader.

private with League.Strings;
private with XML.SAX.Attributes;
private with XML.SAX.Content_Handlers;
private with XML.SAX.Error_Handlers;
with XML.SAX.Input_Sources;
private with XML.SAX.Lexical_Handlers;
private with XML.SAX.Locators;
private with XML.SAX.Parse_Exceptions;
private with XML.SAX.Simple_Readers;

private with VSS.XML.Content_Handlers;
private with VSS.XML.Error_Handlers;
private with VSS.XML.Implementation.Error_Handlers;
private with VSS.XML.Implementation.Matreshka_Locators;
private with VSS.XML.Lexical_Handlers;
with VSS.XML.Readers;

package VSS.XML.Matreshka_Readers is

   type Matreshka_Reader is
     limited new VSS.XML.Readers.SAX_Reader with private;

   procedure Parse
     (Self  : in out Matreshka_Reader'Class;
      Input : in out Standard.XML.SAX.Input_Sources.SAX_Input_Source'Class);
   --  Parse given input source till the end or fatal error and call
   --  subpograms of the configured handlers.
   --
   --  This subprogram accepts Matreshka's input source and do in
   --  non-incremental parsing.

private

   --------------------------
   -- Matreshka_Dispatcher --
   --------------------------

   type Matreshka_Dispatcher is
     limited new Standard.XML.SAX.Content_Handlers.SAX_Content_Handler
       and Standard.XML.SAX.Error_Handlers.SAX_Error_Handler
       and Standard.XML.SAX.Lexical_Handlers.SAX_Lexical_Handler with
   record
      Locator :
        aliased VSS.XML.Implementation.Matreshka_Locators.Matreshka_Locator;
      Content : VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
      Lexical : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access;
      Error   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;
   end record;

   overriding procedure Characters
     (Self    : in out Matreshka_Dispatcher;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure End_Document
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean);

   overriding procedure End_Element
     (Self           : in out Matreshka_Dispatcher;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean);

   overriding procedure End_Prefix_Mapping
     (Self    : in out Matreshka_Dispatcher;
      Prefix  : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding function Error_String
     (Self : Matreshka_Dispatcher) return League.Strings.Universal_String;

   overriding procedure Ignorable_Whitespace
     (Self    : in out Matreshka_Dispatcher;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure Processing_Instruction
     (Self    : in out Matreshka_Dispatcher;
      Target  : League.Strings.Universal_String;
      Data    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure Set_Document_Locator
     (Self    : in out Matreshka_Dispatcher;
      Locator : Standard.XML.SAX.Locators.SAX_Locator);

   overriding procedure Skipped_Entity
     (Self    : in out Matreshka_Dispatcher;
      Name    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure Start_Document
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean);

   overriding procedure Start_Element
     (Self           : in out Matreshka_Dispatcher;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : Standard.XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean);

   overriding procedure Start_Prefix_Mapping
     (Self          : in out Matreshka_Dispatcher;
      Prefix        : League.Strings.Universal_String;
      Namespace_URI : League.Strings.Universal_String;
      Success       : in out Boolean);

   overriding procedure Comment
     (Self    : in out Matreshka_Dispatcher;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure End_CDATA
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean);

   overriding procedure End_DTD
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean);

   overriding procedure End_Entity
     (Self    : in out Matreshka_Dispatcher;
      Name    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure Start_CDATA
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean);

   overriding procedure Start_DTD
     (Self      : in out Matreshka_Dispatcher;
      Name      : League.Strings.Universal_String;
      Public_Id : League.Strings.Universal_String;
      System_Id : League.Strings.Universal_String;
      Success   : in out Boolean);

   overriding procedure Start_Entity
     (Self    : in out Matreshka_Dispatcher;
      Name    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure Error
     (Self       : in out Matreshka_Dispatcher;
      Occurrence : Standard.XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
      Success    : in out Boolean);

   overriding procedure Fatal_Error
     (Self       : in out Matreshka_Dispatcher;
      Occurrence : Standard.XML.SAX.Parse_Exceptions.SAX_Parse_Exception);

   overriding procedure Warning
     (Self       : in out Matreshka_Dispatcher;
      Occurrence : Standard.XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
      Success    : in out Boolean);

   ----------------------
   -- Matreshka_Reader --
   ----------------------

   type Matreshka_Reader is
     limited new VSS.XML.Readers.SAX_Reader with record
      Dispatcher : aliased Matreshka_Dispatcher;
      Reader     : Standard.XML.SAX.Simple_Readers.Simple_Reader;
   end record;

   overriding procedure Set_Content_Handler
     (Self    : in out Matreshka_Reader;
      Handler : VSS.XML.Content_Handlers.SAX_Content_Handler_Access);

   overriding procedure Set_Lexical_Handler
     (Self    : in out Matreshka_Reader;
      Handler : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access);

   overriding procedure Set_Error_Handler
     (Self    : in out Matreshka_Reader;
      Handler : VSS.XML.Error_Handlers.SAX_Error_Handler_Access);

end VSS.XML.Matreshka_Readers;
