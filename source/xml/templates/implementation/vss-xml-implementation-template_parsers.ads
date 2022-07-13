--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Vectors;

private with VSS.IRIs;
private with VSS.Strings;
private with VSS.XML.Attributes;
with VSS.XML.Content_Handlers;
with VSS.XML.Error_Handlers;
with VSS.XML.Implementation.Template_Programs;
private with VSS.XML.Implementation.Error_Handlers;
with VSS.XML.Lexical_Handlers;
private with VSS.XML.Locators;

package VSS.XML.Implementation.Template_Parsers is

   type Template_Parser is
     limited new VSS.XML.Content_Handlers.SAX_Content_Handler
       and VSS.XML.Lexical_Handlers.SAX_Lexical_Handler with private;

   function Program
     (Self : Template_Parser'Class)
      return
        VSS.XML.Implementation.Template_Programs.Instruction_Vectors.Vector;

private

   type State is record
      Start_Address : VSS.XML.Implementation.Template_Programs.Address := 0;
      Instructions  : Natural := 0;
   end record;

   package State_Vectors is new Ada.Containers.Vectors (Positive, State);

   type Template_Parser is
     limited new VSS.XML.Content_Handlers.SAX_Content_Handler
       and VSS.XML.Lexical_Handlers.SAX_Lexical_Handler with
   record
      Error   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;
      Locator : VSS.XML.Locators.SAX_Locator_Access;

      Program :
        VSS.XML.Implementation.Template_Programs.Instruction_Vectors.Vector;

      CDATA   : Boolean := False;
      --  Whether parser is in CDATA state.

      Current : State;
      Stack   : State_Vectors.Vector;
   end record;

   overriding procedure Set_Document_Locator
     (Self    : in out Template_Parser;
      Locator : VSS.XML.Locators.SAX_Locator_Access);

   overriding procedure Start_Document
     (Self    : in out Template_Parser;
      Success : in out Boolean);

   --  procedure End_Document
   --    (Self    : in out SAX_Content_Handler;
   --     Success : in out Boolean) is null;
   --
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
     (Self       : in out Template_Parser;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean);

   overriding procedure End_Element
     (Self    : in out Template_Parser;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Characters
     (Self    : in out Template_Parser;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Ignorable_Whitespace
     (Self    : in out Template_Parser;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) renames Characters;

   overriding procedure Processing_Instruction
     (Self    : in out Template_Parser;
      Target  : VSS.Strings.Virtual_String;
      Data    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Comment
     (Self    : in out Template_Parser;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Start_CDATA
     (Self    : in out Template_Parser;
      Success : in out Boolean);

   overriding procedure End_CDATA
     (Self    : in out Template_Parser;
      Success : in out Boolean);

end VSS.XML.Implementation.Template_Parsers;
