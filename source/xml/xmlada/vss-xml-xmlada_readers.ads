--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Implementation of SAX_Reader on top of XmlAda.

with Input_Sources;
private with Sax.Exceptions;
private with Sax.Locators;
private with Sax.Readers;
private with Sax.Symbols;
private with Sax.Utils;
private with Unicode.CES;

private with VSS.XML.Content_Handlers;
private with VSS.XML.Error_Handlers;
private with VSS.XML.Implementation.Error_Handlers;
private with VSS.XML.Implementation.XmlAda_Locators;
private with VSS.XML.Lexical_Handlers;
with VSS.XML.Readers;

package VSS.XML.XmlAda_Readers is

   type XmlAda_Reader is limited new VSS.XML.Readers.SAX_Reader with private;

   procedure Parse
     (Self  : in out XmlAda_Reader'Class;
      Input : in out Input_Sources.Input_Source'Class);
   --  Parse given input source till the end or fatal error and call
   --  subpograms of the configured handlers.
   --
   --  This subprogram accepts XmlAda's input source.

private

   -----------------------
   -- XmlAda_Dispatcher --
   -----------------------

   type XmlAda_Dispatcher is new Sax.Readers.Sax_Reader with record
      Success : Boolean;
      Locator : aliased VSS.XML.Implementation.XmlAda_Locators.XmlAda_Locator;
      Content : VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
      Lexical : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access;
      Error   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;
   end record;

   overriding procedure Set_Document_Locator
     (Self : in out XmlAda_Dispatcher;
      Loc  : in out Sax.Locators.Locator);

   overriding procedure Start_Document (Self : in out XmlAda_Dispatcher);

   overriding procedure End_Document (Self : in out XmlAda_Dispatcher);

   overriding procedure Start_Prefix_Mapping
     (Self   : in out XmlAda_Dispatcher;
      Prefix : Sax.Symbols.Symbol;
      URI    : Sax.Symbols.Symbol);

   overriding procedure End_Prefix_Mapping
     (Self   : in out XmlAda_Dispatcher;
      Prefix : Sax.Symbols.Symbol);

   overriding procedure Start_Element
     (Self       : in out XmlAda_Dispatcher;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List);

   overriding procedure End_Element
     (Self       : in out XmlAda_Dispatcher;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol);

   overriding procedure Characters
     (Self : in out XmlAda_Dispatcher;
      Ch   : Unicode.CES.Byte_Sequence);

   overriding procedure Ignorable_Whitespace
     (Self : in out XmlAda_Dispatcher;
      Ch   : Unicode.CES.Byte_Sequence);

   overriding procedure Processing_Instruction
     (Self   : in out XmlAda_Dispatcher;
      Target : Unicode.CES.Byte_Sequence;
      Data   : Unicode.CES.Byte_Sequence);

   overriding procedure Skipped_Entity
     (Self : in out XmlAda_Dispatcher;
      Name : Sax.Symbols.Symbol);

   overriding procedure Comment
     (Self : in out XmlAda_Dispatcher;
      Ch   : Unicode.CES.Byte_Sequence);

   overriding procedure Start_Cdata (Self : in out XmlAda_Dispatcher);

   overriding procedure End_Cdata (Self : in out XmlAda_Dispatcher);

   overriding procedure Start_Entity
     (Self : in out XmlAda_Dispatcher;
      Name : Sax.Symbols.Symbol);

   overriding procedure End_Entity
     (Self : in out XmlAda_Dispatcher;
      Name : Sax.Symbols.Symbol);

   overriding procedure Start_DTD
     (Self      : in out XmlAda_Dispatcher;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence);

   overriding procedure End_DTD (Self : in out XmlAda_Dispatcher);

   overriding procedure Warning
     (Self   : in out XmlAda_Dispatcher;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding procedure Error
     (Self   : in out XmlAda_Dispatcher;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding procedure Fatal_Error
     (Self   : in out XmlAda_Dispatcher;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);

   -------------------
   -- XmlAda_Reader --
   -------------------

   type XmlAda_Reader is
     limited new VSS.XML.Readers.SAX_Reader with record
      Dispatcher : XmlAda_Dispatcher;
   end record;

   overriding procedure Set_Content_Handler
     (Self    : in out XmlAda_Reader;
      Handler : VSS.XML.Content_Handlers.SAX_Content_Handler_Access);

   overriding procedure Set_Lexical_Handler
     (Self    : in out XmlAda_Reader;
      Handler : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access);

   overriding procedure Set_Error_Handler
     (Self    : in out XmlAda_Reader;
      Handler : VSS.XML.Error_Handlers.SAX_Error_Handler_Access);

end VSS.XML.XmlAda_Readers;
