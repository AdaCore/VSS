--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Vectors;

with VSS.XML.Content_Handlers;
with VSS.XML.Error_Handlers;
with VSS.XML.Lexical_Handlers;

private with VSS.IRIs;
private with VSS.Strings;
with VSS.Text_Streams;
private with VSS.XML.Attributes;
private with VSS.XML.Implementation.Error_Handlers;
private with VSS.XML.Implementation.HTML_Writer_Data;
private with VSS.XML.Locators;

package VSS.HTML.Writers is

   --  pragma Preelaborate;

   type HTML5_Writer is
     limited new VSS.XML.Content_Handlers.SAX_Content_Handler
       and VSS.XML.Lexical_Handlers.SAX_Lexical_Handler with private;

   procedure Set_Output_Stream
     (Self   : in out HTML5_Writer'Class;
      Stream : VSS.Text_Streams.Output_Text_Stream_Access);

   procedure Set_Error_Handler
     (Self : in out HTML5_Writer'Class;
      To   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access);

private

   type Attribute_Syntax is
     (None, Empty, Unquoted, Single_Quoted, Double_Quoted);
   --  Syntax used for last attribute of the element.

   type Restrictions_Record is record
      --  Text restrictions

      No_Less_Than           : Boolean;
      No_Ambiguous_Ampersand : Boolean;
      No_End_Script          : Boolean;
      No_End_Style           : Boolean;
      No_End_Text_Area       : Boolean;
      No_End_Title           : Boolean;

      --  Content restrictions

      No_Text                : Boolean;
      No_Character_Reference : Boolean;
      No_CDATA               : Boolean;
      No_Element             : Boolean;
      No_Comment             : Boolean;
   end record;

   type Child_Kind is (None, Text, CDATA, Comment, Element);

   type Child_Information (Kind : Child_Kind := None) is record
      case Kind is
         when Element =>
            Element     :
              VSS.XML.Implementation.HTML_Writer_Data.HTML_Element_Kind;
            Tag         : VSS.Strings.Virtual_String;
            End_Omitted : Boolean;

         when others =>
            null;
      end case;
   end record;

   type State_Kinds is (Initial, Element);

   type State_Record (State : State_Kinds := Initial)  is record
      Restrictions : Restrictions_Record;
      --  Restrictions on content of the current item.

      Last_Child   : Child_Information;
      --  Last processed child item.

      case State is
         when Initial =>
            null;

         when Element =>
            Element              :
              VSS.XML.Implementation.HTML_Writer_Data.HTML_Element_Kind;
            Tag                  : VSS.Strings.Virtual_String;

            Syntax               : Attribute_Syntax;
            --  Syntax used for the last attribute of the element.

            Start_Omitted        : Boolean;
            --  Start tag is omitted.

            Start_Closed         : Boolean;
            --  Whether start tag has been closed. It is set to True when
            --  final decision to omit the start tag is given.

            Preserve_Whitespaces : Boolean;
            --  Whether whitespaces of the element must be preserved or not.
      end case;
   end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Positive, State_Record);

   type HTML5_Writer is
     limited new VSS.XML.Content_Handlers.SAX_Content_Handler
       and VSS.XML.Lexical_Handlers.SAX_Lexical_Handler with
   record
      Omit_Whitespaces     : Boolean := True;
      --  Whether whitespaces may be omitted at the begin and end of the
      --  text segment, as well as be squeezed inside the text segment.
      --  This transformation done only when it is allowed:
      --   - HTML element is not one of the known text-only elements
      --     (raw text or escapable text)
      --   - xml:space=preserve is not specified for element or its ancestors
      --   - preserve of whitespaces is not activated by default
      Preserve_Whitespaces : Boolean := False;
      --  Whether whitespace must be preserved for all elements of the
      --  document. Note, this value is used for xml:space=default.

      Output               : VSS.Text_Streams.Output_Text_Stream_Access;
      Error                : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;
      Locator              : VSS.XML.Locators.SAX_Locator_Access;

      Text                 : VSS.Strings.Virtual_String;
      Is_Whitespace        : Boolean;

      CDATA_Mode           : Boolean;
      Current              : State_Record;
      Stack                : State_Vectors.Vector;
   end record;

   overriding procedure Set_Document_Locator
     (Self    : in out HTML5_Writer;
      Locator : VSS.XML.Locators.SAX_Locator_Access);

   overriding procedure Start_Document
     (Self    : in out HTML5_Writer;
      Success : in out Boolean);

   overriding procedure End_Document
     (Self    : in out HTML5_Writer;
      Success : in out Boolean);

   overriding procedure Start_Element
     (Self       : in out HTML5_Writer;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean);

   overriding procedure End_Element
     (Self    : in out HTML5_Writer;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Characters
     (Self    : in out HTML5_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Ignorable_Whitespace
     (Self    : in out HTML5_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) renames Characters;

   --  procedure Processing_Instruction
   --    (Self    : in out SAX_Content_Handler;
   --     Target  : VSS.Strings.Virtual_String;
   --     Data    : VSS.Strings.Virtual_String;
   --     Success : in out Boolean) is null;
   --
   --  procedure Skipped_Entity
   --    (Self    : in out SAX_Content_Handler;
   --     Name    : VSS.Strings.Virtual_String;
   --     Success : in out Boolean) is null;

   overriding procedure Comment
     (Self    : in out HTML5_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Start_CDATA
     (Self    : in out HTML5_Writer;
      Success : in out Boolean);

   overriding procedure End_CDATA
     (Self    : in out HTML5_Writer;
      Success : in out Boolean);

end VSS.HTML.Writers;
