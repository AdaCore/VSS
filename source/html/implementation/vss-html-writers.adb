--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Line_Iterators;
with VSS.XML.Implementation.Parse_Errors;
with VSS.XML.Namespaces;

package body VSS.HTML.Writers is

   use type VSS.IRIs.IRI;
   use all type VSS.XML.Implementation.HTML_Writer_Data.HTML_Element_Kind;

   --  Special kinds of tags
   --
   --    Void: area, base, br, col, command, embed, hr, img, input, keygen,
   --          link, meta, param, source, track, wbr
   --    Raw text: script, style
   --    RCDATA: textarea, title
   --    Foreign: non HTML namespace URI
   --    Normal: all others
   --
   --  Start omittable: html, head, body, colgroup, tbody
   --
   --  End omittable: html, head, body, li, dt, dd, p, rt, rp, optgroup,
   --                 option, colgroup, thead, tbody, tfoot, tr, td, th
   --
   --  Boolean attributes:
   --
   --  allowfullscreen
   --  async
   --  autofocus
   --  autoplay
   --  checked
   --  controls
   --  default
   --  defer
   --  disabled
   --  formnovalidate
   --  ismap
   --  itemscope
   --  loop
   --  multiple
   --  muted
   --  nomodule
   --  novalidate
   --  open
   --  playsinline
   --  readonly
   --  required
   --  reversed
   --  selected
   --  truespeed

   --  Special tags
   --
   --             type      omit       omittable  text  content text
   --                     condition                      first line
   --                                                    convention
   --
   --  a                      p                     t
   --  address                +
   --  area       void
   --  article                +
   --  aside                  +
   --  audio                  p
   --  base       void
   --  blockquote             +
   --  body                                +
   --  br         void
   --  caption                            - +
   --  col        void        +
   --  colgroup               +            +
   --  dd                     +           - +
   --  del                    p
   --  details                +
   --  div                    +
   --  dl                     +
   --  dt                     +           - +
   --  embed      void
   --  fieldset               +
   --  figcaption             +
   --  figure                 +
   --  footer                 +
   --  form                   +
   --  h1                     +
   --  h2                     +
   --  h3                     +
   --  h4                     +
   --  h5                     +
   --  h6                     +
   --  head                                +
   --  header                 +
   --  hgroup                 +
   --  hr         void        +
   --  html                                +
   --  ins                    p
   --  iframe
   --  img        void
   --  input      void
   --  li                     +           - +
   --  link       void        +
   --  main                   +
   --  map                    p
   --  menu                   +
   --  meta       void        +
   --  nav                    +
   --  noscript               p
   --  ol                     +
   --  optgroup               +           - +
   --  option                 +           - +
   --  p                      +           - +
   --  pre                    +                              +
   --  rp                     +           - +
   --  rt                     +           - +
   --  section                +
   --  script     raw         +
   --  source     void
   --  style      raw         +
   --  table                  +
   --  tbody                  +            +
   --  td                     +           - +
   --  template   template    +
   --  textarea   escapable                                  +
   --  tfoot                  +           - +
   --  th                     +           - +
   --  thead                  +           - +
   --  title      escapable
   --  tr                     +           - +
   --  track      void
   --  ul                     +
   --  video                  p
   --  wbr        void

   HTML_DOCTYPE               : constant VSS.Strings.Virtual_String :=
     "<!DOCTYPE html>";
   Comment_Open               : constant VSS.Strings.Virtual_String := "<!--";
   Comment_Close              : constant VSS.Strings.Virtual_String := "-->";
   CDATA_Open                 : constant VSS.Strings.Virtual_String :=
     "<![CDATA[";
   CDATA_Close                : constant VSS.Strings.Virtual_String := "]]>";
   Start_Tag_Open             : constant VSS.Strings.Virtual_String := "<";
   Start_Tag_Close            : constant VSS.Strings.Virtual_String := ">";
   Start_Tag_Self_Close       : constant VSS.Strings.Virtual_String := "/>";
   Start_Tag_Space_Self_Close : constant VSS.Strings.Virtual_String := " />";
   End_Tag_Open               : constant VSS.Strings.Virtual_String := "</";
   End_Tag_Close              : constant VSS.Strings.Virtual_String := ">";

   Ampersand_Reference      : constant VSS.Strings.Virtual_String := "&amp;";
   Apostrophe_Reference     : constant VSS.Strings.Virtual_String := "&apos;";
   Less_Than_Sign_Reference : constant VSS.Strings.Virtual_String := "&lt;";
   Quotation_Mark_Reference : constant VSS.Strings.Virtual_String := "&quot;";

   procedure Write
     (Self    : HTML5_Writer'Class;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);
   --  Write given character to output stream.

   procedure Write
     (Self    : HTML5_Writer'Class;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);
   --  Write given text to output stream.

   procedure Write_New_Line
     (Self    : HTML5_Writer'Class;
      Success : in out Boolean);
   --  Write new line into the output stream.

   procedure Write_Escaped_Text
     (Self    : in out HTML5_Writer'Class;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);
   --  Escape and write given text to output stream.

   procedure Write_Attribute
     (Self    : in out HTML5_Writer'Class;
      Name    : VSS.Strings.Virtual_String;
      Value   : VSS.Strings.Virtual_String;
      Syntax  : out Attribute_Syntax;
      Success : in out Boolean);

   procedure Write_Close_Of_Start_Tag
     (Self    : in out HTML5_Writer'Class;
      Success : in out Boolean);

   type Event_Kind is (Start_Tag, End_Tag, Comment, CDATA);

   type Event_Data (Kind : Event_Kind) is record
      case Kind is
         when Start_Tag =>
            Element :
              VSS.XML.Implementation.HTML_Writer_Data.HTML_Element_Kind;

         when others =>
            null;
      end case;
   end record;

   procedure Close_Current_Tag
     (Self    : in out HTML5_Writer'Class;
      Event   : Event_Data;
      Success : in out Boolean);
   --  Generate close of the current tag when necessary.

   procedure Report_Warning
     (Self    : in out HTML5_Writer'Class;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   procedure Report_Error
     (Self    : in out HTML5_Writer'Class;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   --  procedure Report_Fatal_Error
   --    (Self    : in out HTML5_Writer'Class;
   --     Message : VSS.Strings.Virtual_String;
   --     Success : in out Boolean);

   function Is_ASCII_Whitespace
     (Item : VSS.Characters.Virtual_Character) return Boolean;
   function Is_ASCII_Whitespace
     (Item : VSS.Strings.Virtual_String) return Boolean;
   --  Return True when string contains only ASCII whitespace characters.

   function HTML_Element_Restrictions
     (Element    : VSS.XML.Implementation.HTML_Writer_Data.HTML_Element_Kind;
      Properties : VSS.XML.Implementation.HTML_Writer_Data.Element_Properties;
      Parent     : Restrictions_Record) return Restrictions_Record;
   --  Compute restrictions of the element.

   function Best_Attribute_Syntax
     (Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String) return Attribute_Syntax;
   --  Computes most appropriate syntax for the given attribute's value. If
   --  Name is not an empty string it is checked for the name of the know
   --  boolean attributes, and empty syntax is used for them.

   ---------------------------
   -- Best_Attribute_Syntax --
   ---------------------------

   function Best_Attribute_Syntax
     (Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String) return Attribute_Syntax
   is
      Single  : Natural := 0;
      Double  : Natural := 0;
      Special : Natural := 0;

   begin
      if Value.Is_Empty
        or else (not Name.Is_Empty
                   and then VSS.XML.Implementation.HTML_Writer_Data
                              .Is_Boolean_Attribute (Name))
      then
         return Empty;
      end if;

      declare
         Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
           Value.Before_First_Character;

      begin
         while Iterator.Forward loop
            case Iterator.Element is
               when VSS.Characters.Latin.Character_Tabulation
                  | VSS.Characters.Latin.Line_Feed
                  | VSS.Characters.Latin.Form_Feed
                  | VSS.Characters.Latin.Carriage_Return
                  | VSS.Characters.Latin.Space
                  | VSS.Characters.Latin.Equals_Sign
                  | VSS.Characters.Latin.Less_Than_Sign
                  | VSS.Characters.Latin.Greater_Than_Sign
                  | VSS.Characters.Latin.Grave_Accent
                  =>
                  Special := @ + 1;

               when VSS.Characters.Latin.Quotation_Mark =>
                  Double := @ + 1;

               when VSS.Characters.Latin.Apostrophe =>
                  Single := @ + 1;

               when others =>
                  null;
            end case;
         end loop;

         if Special + Single + Double = 0 then
            return Unquoted;

         elsif Single <= Double then
            return Single_Quoted;

         else
            return Double_Quoted;
         end if;
      end;
   end Best_Attribute_Syntax;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out HTML5_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      Is_Whitespace : constant Boolean := Is_ASCII_Whitespace (Text);

   begin
      if Self.CDATA_Mode then
         --  In CDATA mode text is written to output immidiately: CDATA is
         --  not allowed in HTML, thus there is no enough information to do
         --  any transformations.

         Self.Write_Escaped_Text (Text, Success);

      else
         if Self.Current.Restrictions.No_Text then
            --  Report warning for the first text non-whitespace segment

            if not Is_Whitespace and Self.Is_Whitespace then
               Self.Report_Warning ("Text is not allowed", Success);
            end if;

            Self.Is_Whitespace := @ and Is_Whitespace;
         end if;

         Self.Text.Append (Text);
      end if;
   end Characters;

   -----------------------
   -- Close_Current_Tag --
   -----------------------

   procedure Close_Current_Tag
     (Self    : in out HTML5_Writer'Class;
      Event   : Event_Data;
      Success : in out Boolean) is
   begin
      --  Reject ignorable whitespaces.

      if not Self.Text.Is_Empty
        and then Self.Omit_Whitespaces
      then
         if Self.Is_Whitespace
           and Self.Current.Restrictions.No_Text
         then
            Self.Text.Clear;
            Self.Is_Whitespace := True;

         elsif not Self.Current.Preserve_Whitespaces then
            declare
               Aux      : constant VSS.Strings.Virtual_String := Self.Text;
               Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
                 Aux.Before_First_Character;
               Omitted  : Boolean := False;

            begin
               Self.Text.Clear;
               --  Self.Text.Set_Capacity (Aux.Character_Length);
               --  ??? Virtual_String.Set_Capacity is not implemented,
               --  however, setting of it may improve performance on long
               --  strings.

               while Iterator.Forward loop
                  declare
                     C : constant VSS.Characters.Virtual_Character :=
                       Iterator.Element;

                  begin
                     if Is_ASCII_Whitespace (C) then
                        if not Self.Text.Is_Empty then
                           Omitted := True;
                        end if;

                     else
                        if Omitted then
                           Self.Text.Append (VSS.Characters.Latin.Space);
                           Omitted := False;
                        end if;

                        Self.Text.Append (C);
                     end if;
                  end;
               end loop;
            end;
         end if;
      end if;

      --  Check whether end tag of the last processed element may be
      --  omitted.

      if Self.Current.Last_Child.Kind = Element
        and then Self.Current.Last_Child.End_Omitted
      then
         declare
            Properties : constant
              VSS.XML.Implementation.HTML_Writer_Data.Element_Properties :=
                VSS.XML.Implementation.HTML_Writer_Data.Properties
                  (Self.Current.Last_Child.Element);
            Omit       : Boolean := False;

         begin
            if Self.Text.Is_Empty then
               case Event.Kind is
                  when Start_Tag =>
                     Omit :=
                       VSS.XML.Implementation.HTML_Writer_Data.Element
                         (Properties.End_Tag.Next_Sibling,
                          Event.Element);

                  when End_Tag =>
                     Omit :=
                       VSS.XML.Implementation.HTML_Writer_Data.Value
                         (Properties.End_Tag.End_Of_Parent,
                          Self.Current.Last_Child.Element);

                  when Comment =>
                     Omit := Properties.End_Tag.Next_Sibling.Comment;

                  when CDATA =>
                     null;
               end case;

            else
               if Is_ASCII_Whitespace
                 (Self.Text.At_First_Character.Element)
               then
                  Omit := Properties.End_Tag.Next_Sibling.Whitespace;

               else
                  Omit := Properties.End_Tag.Next_Sibling.Text;
               end if;
            end if;

            if not Omit then
               Self.Write (End_Tag_Open, Success);
               Self.Write (Self.Current.Last_Child.Tag, Success);
               Self.Write (End_Tag_Close, Success);
            end if;
         end;
      end if;

      --  Check whether start tag of the current element may be omitted.

      if Self.Current.State /= Initial then
         if not Self.Current.Start_Closed then
            if Self.Current.Start_Omitted then
               declare
                  Properties : constant
                    VSS.XML.Implementation.HTML_Writer_Data.Element_Properties
                      := VSS.XML.Implementation.HTML_Writer_Data.Properties
                        (Self.Current.Element);
                  Omit       : Boolean := False;

               begin
                  if Self.Text.Is_Empty then
                     case Event.Kind is
                        when Start_Tag =>
                           Omit :=
                             VSS.XML.Implementation.HTML_Writer_Data.Element
                               (Properties.Start_Tag.First_Child,
                                Event.Element);

                        when End_Tag =>
                           Omit :=
                             Properties.Start_Tag.Is_Empty
                               and Self.Current.Last_Child.Kind = None;

                        when Comment =>
                           Omit := Properties.Start_Tag.First_Child.Comment;

                        when CDATA =>
                           null;
                     end case;

                  else
                     if Is_ASCII_Whitespace
                       (Self.Text.At_First_Character.Element)
                     then
                        Omit := Properties.Start_Tag.First_Child.Whitespace;

                     else
                        Omit := Properties.Start_Tag.First_Child.Text;
                     end if;
                  end if;

                  if Omit then
                     Self.Current.Start_Omitted := True;
                     Self.Current.Start_Closed  := True;

                  else
                     Self.Write (Start_Tag_Open, Success);
                     Self.Write (Self.Current.Tag, Success);
                     Self.Current.Start_Omitted := False;

                     if Event.Kind /= End_Tag then
                        Self.Write_Close_Of_Start_Tag (Success);
                     end if;
                  end if;
               end;

            else
               if Event.Kind /= End_Tag then
                  Self.Write_Close_Of_Start_Tag (Success);
               end if;
            end if;
         end if;
      end if;

      --  Write accumulated text if any.

      if not Self.Text.Is_Empty then
         if not Self.Current.Start_Closed then
            Self.Write_Close_Of_Start_Tag (Success);
         end if;

         if Self.Current.Element in pre_Element | textarea_Element
           and then Self.Current.Last_Child.Kind = None
         then
            declare
               use type VSS.Strings.Character_Count;

               Iterator : constant VSS.Strings.Line_Iterators.Line_Iterator :=
                 Self.Text.At_First_Line
                   (VSS.XML.Implementation.HTML_Writer_Data
                      .HTML_New_Line_Function);

            begin
               if Iterator.Character_Length = 0 then
                  --  First line of the text is empty, thus additional empty
                  --  line should be added in HTML serialization format.

                  Self.Write (VSS.Characters.Latin.Line_Feed, Success);
               end if;
            end;
         end if;

         Self.Write_Escaped_Text (Self.Text, Success);

         Self.Text.Clear;
         Self.Is_Whitespace := True;
      end if;
   end Close_Current_Tag;

   -------------
   -- Comment --
   -------------

   overriding procedure Comment
     (Self    : in out HTML5_Writer;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      Self.Close_Current_Tag ((Kind => Comment), Success);

      if Self.Current.Restrictions.No_Comment then
         raise Program_Error;
      end if;

      --  XXX '--' must not be present in the comment, check and report?

      Self.Write (Comment_Open, Success);
      --  Self.Write_Escaped_Text (Text, Comment, Success);
      Self.Write (Text, Success);
      Self.Write (Comment_Close, Success);

      if Self.Current.State = Initial then
         --  "It is suggested that newlines be inserted ... after any comments
         --  that are before the document element"

         Self.Write_New_Line (Success);
      end if;

      Self.Current.Last_Child := (Kind => Comment);
   end Comment;

   ---------------
   -- End_CDATA --
   ---------------

   overriding procedure End_CDATA
     (Self    : in out HTML5_Writer;
      Success : in out Boolean) is
   begin
      Self.CDATA_Mode := False;

      if not Self.Current.Restrictions.No_CDATA then
         Self.Write (CDATA_Close, Success);
      end if;
   end End_CDATA;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document
     (Self    : in out HTML5_Writer;
      Success : in out Boolean) is
   begin
      null;
      --  raise Program_Error;
   end End_Document;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self    : in out HTML5_Writer;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      use all type VSS.XML.Implementation.HTML_Writer_Data.Element_Kinds;

      Current    : constant State_Record := Self.Current;
      Properties : VSS.XML.Implementation.HTML_Writer_Data.Element_Properties
        renames VSS.XML.Implementation.HTML_Writer_Data.Properties
                  (Self.Current.Element);

   begin
      Self.Close_Current_Tag ((Kind => End_Tag), Success);

      if not Self.Current.Start_Closed
        and then Self.Current.Element = Foreign
      then
         --  For empty foreign elements use self-closing sintax when element
         --  is empty.

         if Self.Current.Syntax = Unquoted then
            --  When unquoted syntax is used for attributes, SOLIDUS '/'
            --  character must be separated from the value of the last
            --  attribue by the whitespace.

            Self.Write (Start_Tag_Space_Self_Close, Success);

         else
            Self.Write (Start_Tag_Self_Close, Success);
         end if;

      elsif not Self.Current.Start_Closed
        and then Properties.Kind = Void
      then
         --  For void HTML elements just close start tag. Self-closing is not
         --  recommended, and closing tag is not needed (and not desirable for
         --  some browsers).

         Self.Write_Close_Of_Start_Tag (Success);

      else
         if not Self.Current.Start_Closed then
            Self.Write_Close_Of_Start_Tag (Success);
         end if;

         if not Properties.End_Tag.May_Be_Omitted then
            Self.Write (End_Tag_Open, Success);
            Self.Write (Name, Success);
            Self.Write (End_Tag_Close, Success);
         end if;
      end if;

      Self.Current := Self.Stack.Last_Element;
      Self.Stack.Delete_Last;

      Self.Current.Last_Child :=
        (Kind        => Element,
         Element     => Current.Element,
         Tag         => Current.Tag,
         End_Omitted => Properties.End_Tag.May_Be_Omitted);
   end End_Element;

   -------------------------------
   -- HTML_Element_Restrictions --
   -------------------------------

   function HTML_Element_Restrictions
     (Element    : VSS.XML.Implementation.HTML_Writer_Data.HTML_Element_Kind;
      Properties : VSS.XML.Implementation.HTML_Writer_Data.Element_Properties;
      Parent     : Restrictions_Record) return Restrictions_Record
   is
      use all type VSS.XML.Implementation.HTML_Writer_Data.Element_Kinds;
      use all type VSS.XML.Implementation.HTML_Writer_Data.Text_Children;

   begin
      return Result : Restrictions_Record := (others => False) do
         Result.No_Less_Than           := True;
         Result.No_Ambiguous_Ampersand := True;
         Result.No_CDATA               := True;

         case Properties.Kind is
            when Void =>
               pragma Assert (Properties.Text = No);
               --  Void element can't contain anything, including text.

               Result.No_Text                := True;
               Result.No_Character_Reference := True;
               Result.No_Element             := True;
               Result.No_Comment             := True;

            when Template =>
               raise Program_Error;

            when Raw_Text =>
               pragma Assert (Properties.Text = Yes);
               --  Raw text element contains text.

               Result.No_Less_Than           := False;
               Result.No_Ambiguous_Ampersand := False;
               Result.No_Character_Reference := True;
               Result.No_Element             := True;
               Result.No_Comment             := True;

               case Element is
                  when script_Element =>
                     Result.No_End_Script := True;

                  when style_Element =>
                     Result.No_End_Style := True;

                  when others =>
                     raise Program_Error;
               end case;

            when Escapable_Text =>
               pragma Assert (Properties.Text = Yes);
               --  Escapable text element contains text.

               Result.No_Less_Than := False;
               Result.No_Element   := True;
               Result.No_Comment   := True;

               case Element is
                  when textarea_Element =>
                     Result.No_End_Text_Area := True;

                  when title_Element =>
                     Result.No_End_Title := True;

                  when others =>
                     raise Program_Error;
               end case;

            when Normal =>
               Result.No_Less_Than           := True;
               Result.No_Ambiguous_Ampersand := True;

               case Properties.Text is
                  when No =>
                     Result.No_Text := True;

                  when Transparent =>
                     Result.No_Text := Parent.No_Text;

                  when Yes =>
                     null;
               end case;

            when Foreign =>
               Result.No_CDATA := False;
         end case;
      end return;
   end HTML_Element_Restrictions;

   -------------------------
   -- Is_ASCII_Whitespace --
   -------------------------

   function Is_ASCII_Whitespace
     (Item : VSS.Characters.Virtual_Character) return Boolean is
   begin
      return
        Item in VSS.Characters.Latin.Character_Tabulation
              | VSS.Characters.Latin.Line_Feed
              | VSS.Characters.Latin.Form_Feed
              | VSS.Characters.Latin.Carriage_Return
              | VSS.Characters.Latin.Space;
   end Is_ASCII_Whitespace;

   ------------------------
   -- Is_HTML_Whitespace --
   ------------------------

   function Is_ASCII_Whitespace
     (Item : VSS.Strings.Virtual_String) return Boolean
   is
      Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
        Item.Before_First_Character;

   begin
      while Iterator.Forward loop
         case Iterator.Element is
            when VSS.Characters.Latin.Character_Tabulation
               | VSS.Characters.Latin.Line_Feed
               | VSS.Characters.Latin.Form_Feed
               | VSS.Characters.Latin.Carriage_Return
               | VSS.Characters.Latin.Space
               =>
               null;

            when others =>
               return False;
         end case;
      end loop;

      return True;
   end Is_ASCII_Whitespace;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error
     (Self    : in out HTML5_Writer'Class;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      use type VSS.XML.Error_Handlers.SAX_Error_Handler_Access;

      Error : constant VSS.XML.Implementation.Parse_Errors.Parse_Error :=
        (Self.Locator, Message);

   begin
      if Self.Error /= null then
         Self.Error.Error (Error, Success);
      end if;
   end Report_Error;

   ------------------------
   -- Report_Fatal_Error --
   ------------------------

   --  procedure Report_Fatal_Error
   --    (Self    : in out HTML5_Writer'Class;
   --     Message : VSS.Strings.Virtual_String;
   --     Success : in out Boolean)
   --  is
   --     use type VSS.XML.Error_Handlers.SAX_Error_Handler_Access;
   --
   --     Error : constant VSS.XML.Implementation.Parse_Errors.Parse_Error :=
   --       (Self.Locator, Message);
   --
   --  begin
   --     if Self.Error /= null then
   --        Self.Error.Fatal_Error (Error, Success);
   --     end if;
   --  end Report_Fatal_Error;

   --------------------
   -- Report_Warning --
   --------------------

   procedure Report_Warning
     (Self    : in out HTML5_Writer'Class;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      use type VSS.XML.Error_Handlers.SAX_Error_Handler_Access;

      Error : constant VSS.XML.Implementation.Parse_Errors.Parse_Error :=
        (Self.Locator, Message);

   begin
      if Self.Error /= null then
         Self.Error.Warning (Error, Success);
      end if;
   end Report_Warning;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Self    : in out HTML5_Writer;
      Locator : VSS.XML.Locators.SAX_Locator_Access) is
   begin
      Self.Locator := Locator;
   end Set_Document_Locator;

   -----------------------
   -- Set_Error_Handler --
   -----------------------

   procedure Set_Error_Handler
     (Self : in out HTML5_Writer'Class;
      To   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access) is
   begin
      Self.Error := To;
   end Set_Error_Handler;

   -----------------------
   -- Set_Output_Stream --
   -----------------------

   procedure Set_Output_Stream
     (Self   : in out HTML5_Writer'Class;
      Stream : VSS.Text_Streams.Output_Text_Stream_Access) is
   begin
      Self.Output := Stream;
   end Set_Output_Stream;

   -----------------
   -- Start_CDATA --
   -----------------

   overriding procedure Start_CDATA
     (Self    : in out HTML5_Writer;
      Success : in out Boolean) is
   begin
      Self.Close_Current_Tag ((Kind => CDATA), Success);
      Self.CDATA_Mode := True;

      if Self.Current.Restrictions.No_CDATA then
         Self.Report_Warning ("CDATA is not allowed", Success);

      else
         Self.Write (CDATA_Open, Success);
      end if;
   end Start_CDATA;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
     (Self    : in out HTML5_Writer;
      Success : in out Boolean) is
   begin
      Self.Stack.Clear;
      Self.Current :=
        (State                => Initial,
         Restrictions         =>
           (No_Ambiguous_Ampersand | No_Less_Than => True,
            No_Text | No_CDATA                    => True,
            others                                => False),
         Last_Child           => (Kind => None));
      Self.CDATA_Mode := False;
      Self.Text.Clear;
      Self.Is_Whitespace := True;

      Self.Write (HTML_DOCTYPE, Success);

      --  "It is suggested that newlines be inserted after the DOCTYPE"

      Self.Write_New_Line (Success);
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self       : in out HTML5_Writer;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean)
   is
      use type VSS.Strings.Virtual_String;

      Is_HTML_Namespace   : constant Boolean :=
        URI = VSS.XML.Namespaces.HTML_Namespace;
      Is_MathML_Namespace : constant Boolean :=
        URI = VSS.XML.Namespaces.MathML_Namespace;
      Is_SVG_Namespace    : constant Boolean :=
        URI = VSS.XML.Namespaces.SVG_Namespace;

      Current_Element : constant
        VSS.XML.Implementation.HTML_Writer_Data.HTML_Element_Kind :=
          (if Is_HTML_Namespace
           then
              VSS.XML.Implementation.HTML_Writer_Data.To_HTML_Element (Name)
           else Foreign);
      Properties      : constant
        VSS.XML.Implementation.HTML_Writer_Data.Element_Properties :=
          VSS.XML.Implementation.HTML_Writer_Data.Properties
            (Current_Element);
      Has_Attributes  : Boolean := False;
      Has_XML_Space   : Boolean := False;

   begin
      Self.Close_Current_Tag
        ((Kind => Start_Tag, Element => Current_Element), Success);

      if Self.Current.State = Initial
        and then not Is_HTML_Namespace
      then
         --  Self.Fatal_Error ("Not an HTML document");

         raise Program_Error;
      end if;

      if Self.Current.State = Initial
        and then Current_Element /= html_Element
      then
         --  Self.Fatal_Error ("root element must be 'html'");

         raise Program_Error;
      end if;

      if not Is_HTML_Namespace
        and not Is_MathML_Namespace
        and not Is_SVG_Namespace
      then
         --  Self.Error
         --    ("only elements from HTML, MathML and SVG namespaces allowed");

         raise Program_Error;
      end if;

      Self.Stack.Append (Self.Current);
      Self.Current :=
        (State                => Element,
         Element              => Current_Element,
         Tag                  => Name,
         Restrictions         =>
           HTML_Element_Restrictions
             (Current_Element, Properties, Self.Current.Restrictions),
         Syntax               => None,
         Preserve_Whitespaces =>
           Properties.Kind
             in VSS.XML.Implementation.HTML_Writer_Data.Raw_Text
                  | VSS.XML.Implementation.HTML_Writer_Data.Escapable_Text
             or else
               (if Self.Stack.Last_Element.State = Initial
                  then Self.Preserve_Whitespaces
                  else Self.Stack.Last_Element.Preserve_Whitespaces),
         Start_Omitted        => False,
         Start_Closed         => False,
         Last_Child           => (others => <>));

      --  Check list of attributes for xml:space attribute and apply it if
      --  present.

      for J in 1 .. Attributes.Get_Length loop
         if Attributes.Get_URI (J) = XML.Namespaces.XML_Namespace
           and then Attributes.Get_Name (J) = "space"
         then
            Has_XML_Space := True;

            if Current_Element = Foreign then
               Has_Attributes := True;
            end if;

            if Attributes.Get_Value (J) = "default" then
               Self.Current.Preserve_Whitespaces :=
                 Self.Preserve_Whitespaces;

            elsif Attributes.Get_Value (J) = "preserve" then
               Self.Current.Preserve_Whitespaces := True;
            end if;

         else
            Has_Attributes := True;
         end if;
      end loop;

      --  Chech whether start tag of the current element may be omitted.

      if not Has_Attributes
        and then Properties.Start_Tag.May_Be_Omitted
      then
         Self.Current.Start_Omitted := True;

         declare
            Parent : State_Record renames Self.Stack.Last_Element;

         begin
            if Parent.State /= Initial
              and then Parent.Last_Child.Kind = Element
              and then Parent.Last_Child.End_Omitted
              and then Parent.Last_Child.Element
                in Properties.Start_Tag.Previous_Sibling_End_Omitted'Range
            then
               Self.Current.Start_Omitted :=
                 Properties.Start_Tag.Previous_Sibling_End_Omitted
                   (Parent.Last_Child.Element);
            end if;
         end;
      end if;

      --  Write start tag (but not close it) when it can't be omitted.

      if not Self.Current.Start_Omitted then
         Self.Write (Start_Tag_Open, Success);
         Self.Write (Name, Success);

         if Has_Attributes then
            for J in 1 .. Attributes.Get_Length loop
               --  This is simple implementation, no checks or support for
               --  namespaces, especially for allowed namespaces in foreign
               --  elements.

               if not Has_XML_Space
                 or else (Attributes.Get_URI (J)
                            /= XML.Namespaces.XML_Namespace
                          or else Attributes.Get_Name (J) /= "space")
               then
                  Self.Write_Attribute
                    (Name    => Attributes.Get_Name (J),
                     Value   => Attributes.Get_Value (J),
                     Syntax  => Self.Current.Syntax,
                     Success => Success);

               else
                  --  Processing of xml:space attribute.

                  if Self.Current.Element = Foreign then
                     Self.Write_Attribute
                       (Name    => "xml:space",
                        Value   => Attributes.Get_Value (J),
                        Syntax  => Self.Current.Syntax,
                        Success => Success);
                  end if;
               end if;
            end loop;
         end if;
      end if;
   end Start_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self    : HTML5_Writer'Class;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      use type VSS.Text_Streams.Output_Text_Stream_Access;

   begin
      if Self.Output /= null then
         Self.Output.Put (Item, Success);

      else
         --  Success := False;

         raise Program_Error;
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self    : HTML5_Writer'Class;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      use type VSS.Text_Streams.Output_Text_Stream_Access;

   begin
      if Self.Output /= null then
         declare
            Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
              Item.Before_First_Character;

         begin
            while Iterator.Forward loop
               Self.Output.Put (Iterator.Element, Success);
            end loop;
         end;

      else
         --  Success := False;

         raise Program_Error;
      end if;
   end Write;

   ---------------------
   -- Write_Attribute --
   ---------------------

   procedure Write_Attribute
     (Self    : in out HTML5_Writer'Class;
      Name    : VSS.Strings.Virtual_String;
      Value   : VSS.Strings.Virtual_String;
      Syntax  : out Attribute_Syntax;
      Success : in out Boolean)
   is
      use type VSS.Text_Streams.Output_Text_Stream_Access;

      procedure Write_Unquoted_Value;

      procedure Write_Single_Quoted_Value;

      procedure Write_Double_Quoted_Value;

      -------------------------------
      -- Write_Double_Quoted_Value --
      -------------------------------

      procedure Write_Double_Quoted_Value is
         Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
           Value.Before_First_Character;

      begin
         while Iterator.Forward loop
            declare
               C : constant VSS.Characters.Virtual_Character :=
                 Iterator.Element;

            begin
               case C is
                  when VSS.Characters.Latin.Ampersand =>
                     Self.Write (Ampersand_Reference, Success);

                  when VSS.Characters.Latin.Quotation_Mark =>
                     Self.Write (Quotation_Mark_Reference, Success);

                  when others =>
                     Self.Output.Put (C, Success);
               end case;
            end;
         end loop;
      end Write_Double_Quoted_Value;

      -------------------------------
      -- Write_Single_Quoted_Value --
      -------------------------------

      procedure Write_Single_Quoted_Value is
         Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
           Value.Before_First_Character;

      begin
         while Iterator.Forward loop
            declare
               C : constant VSS.Characters.Virtual_Character :=
                 Iterator.Element;

            begin
               case C is
                  when VSS.Characters.Latin.Ampersand =>
                     Self.Write (Ampersand_Reference, Success);

                  when VSS.Characters.Latin.Apostrophe =>
                     Self.Write (Apostrophe_Reference, Success);

                  when others =>
                     Self.Output.Put (C, Success);
               end case;
            end;
         end loop;
      end Write_Single_Quoted_Value;

      --------------------------
      -- Write_Unquoted_Value --
      --------------------------

      procedure Write_Unquoted_Value is
         use type VSS.Characters.Virtual_Character;

         Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
           Value.Before_First_Character;

      begin
         while Iterator.Forward loop
            declare
               C : constant VSS.Characters.Virtual_Character :=
                 Iterator.Element;

            begin
               if C = VSS.Characters.Latin.Ampersand then
                  Self.Write (Ampersand_Reference, Success);

               else
                  Self.Output.Put (C, Success);
               end if;
            end;
         end loop;
      end Write_Unquoted_Value;

   begin
      if Self.Output /= null then
         Syntax :=
           Best_Attribute_Syntax
             ((if Self.Current.Element = Foreign
                 then VSS.Strings.Empty_Virtual_String
                 else Name),
              Value);

         Self.Write (VSS.Characters.Latin.Space, Success);
         Self.Write (Name, Success);

         case Syntax is
            when None =>
               null;

            when Empty =>
               null;

            when Unquoted =>
               Self.Write (VSS.Characters.Latin.Equals_Sign, Success);
               Write_Unquoted_Value;

            when Single_Quoted =>
               Self.Write (VSS.Characters.Latin.Equals_Sign, Success);
               Self.Write (VSS.Characters.Latin.Apostrophe, Success);
               Write_Single_Quoted_Value;
               Self.Write (VSS.Characters.Latin.Apostrophe, Success);

            when Double_Quoted =>
               Self.Write (VSS.Characters.Latin.Equals_Sign, Success);
               Self.Write (VSS.Characters.Latin.Quotation_Mark, Success);
               Write_Double_Quoted_Value;
               Self.Write (VSS.Characters.Latin.Quotation_Mark, Success);
         end case;

      else
         --  Success := False;

         raise Program_Error;
      end if;
   end Write_Attribute;

   ------------------------------
   -- Write_Close_Of_Start_Tag --
   ------------------------------

   procedure Write_Close_Of_Start_Tag
     (Self    : in out HTML5_Writer'Class;
      Success : in out Boolean) is
   begin
      pragma Assert (not Self.Current.Start_Omitted);
      pragma Assert (not Self.Current.Start_Closed);

      --  XXX space after attribute is not supported

      Self.Write (Start_Tag_Close, Success);
      Self.Current.Start_Closed := True;

      if Self.Current.Element = html_Element then
         --  "It is suggested that newlines be inserted ... after the html
         --  element's start tag (if it is not omitted)"

         Self.Write_New_Line (Success);
      end if;
   end Write_Close_Of_Start_Tag;

   ------------------------
   -- Write_Escaped_Text --
   ------------------------

   procedure Write_Escaped_Text
     (Self    : in out HTML5_Writer'Class;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      use type VSS.Characters.Virtual_Character;
      use type VSS.Text_Streams.Output_Text_Stream_Access;

   begin
      if Self.Output /= null then
         declare
            Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
              Item.Before_First_Character;

         begin
            while Iterator.Forward loop
               declare
                  C : constant VSS.Characters.Virtual_Character :=
                    Iterator.Element;

               begin
                  if Self.CDATA_Mode then
                     Self.Output.Put (C, Success);

                  else
                     if C = VSS.Characters.Latin.Less_Than_Sign
                       and Self.Current.Restrictions.No_Less_Than
                     then
                        if Self.Current.Restrictions.No_Character_Reference
                        then
                           Self.Report_Error
                             (VSS.Strings.To_Virtual_String
                                ("LESS-THAN SIGN in the text and character"
                                 & " references are not allowed"),
                              Success);

                           Self.Output.Put (C, Success);

                        else
                           Self.Write (Less_Than_Sign_Reference, Success);
                        end if;

                     elsif C = VSS.Characters.Latin.Ampersand
                       and Self.Current.Restrictions.No_Ambiguous_Ampersand
                     then
                        if Self.Current.Restrictions.No_Character_Reference
                        then
                           --  XXX Check may be added for ambiguous ampersand
                           --  to avoid false negatives of simplified check

                           Self.Report_Error
                             (VSS.Strings.To_Virtual_String
                                ("AMPERSAND in the text and character"
                                 & " references are not allowed"),
                              Success);

                           Self.Output.Put (C, Success);

                        else
                           Self.Write (Ampersand_Reference, Success);
                        end if;

                     else
                        Self.Output.Put (C, Success);
                     end if;
                  end if;
               end;
            end loop;
         end;

      else
         --  Success := False;

         raise Program_Error;
      end if;
   end Write_Escaped_Text;

   --------------------
   -- Write_New_Line --
   --------------------

   procedure Write_New_Line
     (Self    : HTML5_Writer'Class;
      Success : in out Boolean)
   is
      use type VSS.Text_Streams.Output_Text_Stream_Access;

   begin
      if Self.Output /= null then
         Self.Output.Put (VSS.Characters.Latin.Line_Feed, Success);

      else
         --  Success := False;

         raise Program_Error;
      end if;
   end Write_New_Line;

end VSS.HTML.Writers;
