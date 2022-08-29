--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.String_Vectors;
with VSS.XML.Implementation.Parse_Errors;
with VSS.XML.Namespaces;

package body VSS.XML.Implementation.Template_Parsers is

   attributes_Attribute : constant VSS.Strings.Virtual_String := "attributes";
   condition_Attribute  : constant VSS.Strings.Virtual_String := "condition";
   content_Attribute    : constant VSS.Strings.Virtual_String := "content";
   omit_tag_Attribute   : constant VSS.Strings.Virtual_String := "omit-tag";
   repeat_Attribute     : constant VSS.Strings.Virtual_String := "repeat";

   text_Keyword      : constant VSS.Strings.Virtual_String := "text";
   structure_Keyword : constant VSS.Strings.Virtual_String := "structure";

   procedure Report_Error
     (Self    : Template_Parser'Class;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out Template_Parser;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      use type VSS.XML.Implementation.Template_Programs.Instruction_Kind;

      pragma Unreferenced (Success);

   begin
      if Self.Program.Last_Element.Kind
           = VSS.XML.Implementation.Template_Programs.Text
        and then Self.Program.Last_Element.CDATA = Self.CDATA
      then
         declare
            Aux : VSS.XML.Implementation.Template_Programs.Instruction :=
              Self.Program.Last_Element;

         begin
            Aux.Text.Append (Text);
            Self.Program.Replace_Element (Self.Program.Last_Index, Aux);
         end;

      else
         Self.Program.Append
           (VSS.XML.Implementation.Template_Programs.Instruction'
              (Kind  => VSS.XML.Implementation.Template_Programs.Text,
               CDATA => Self.CDATA,
               Text  => Text));
      end if;
   end Characters;

   -------------
   -- Comment --
   -------------

   overriding procedure Comment
     (Self    : in out Template_Parser;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Self.Program.Append
        (VSS.XML.Implementation.Template_Programs.Instruction'
           (VSS.XML.Implementation.Template_Programs.Comment, Text));
   end Comment;

   ---------------
   -- End_CDATA --
   ---------------

   overriding procedure End_CDATA
     (Self    : in out Template_Parser;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Self.CDATA := False;
   end End_CDATA;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self    : in out Template_Parser;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Self.Program.Append
        (VSS.XML.Implementation.Template_Programs.Instruction'
           (VSS.XML.Implementation.Template_Programs.End_Element,
            Self.Current.Start_Address));

      for J in 1 .. Self.Current.Instructions loop
         Self.Program.Append
           (VSS.XML.Implementation.Template_Programs.Instruction'
              (Kind => VSS.XML.Implementation.Template_Programs.Done));
      end loop;

      Self.Current := Self.Stack.Last_Element;
      Self.Stack.Delete_Last;
   end End_Element;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   overriding procedure Processing_Instruction
     (Self    : in out Template_Parser;
      Target  : VSS.Strings.Virtual_String;
      Data    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Self.Program.Append
        (VSS.XML.Implementation.Template_Programs.Instruction'
           (VSS.XML.Implementation.Template_Programs.Processing_Instruction,
            Target,
            Data));
   end Processing_Instruction;

   -------------
   -- Program --
   -------------

   function Program
     (Self : Template_Parser'Class)
      return
        VSS.XML.Implementation.Template_Programs.Instruction_Vectors.Vector is
   begin
      return Self.Program;
   end Program;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error
     (Self    : Template_Parser'Class;
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

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Self    : in out Template_Parser;
      Locator : VSS.XML.Locators.SAX_Locator_Access) is
   begin
      Self.Locator := Locator;
   end Set_Document_Locator;

   -----------------
   -- Start_CDATA --
   -----------------

   overriding procedure Start_CDATA
     (Self    : in out Template_Parser;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Self.CDATA := True;
   end Start_CDATA;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
     (Self    : in out Template_Parser;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      --  Reset state of the parser

      Self.Program.Clear;
      Self.CDATA := False;
      Self.Current := (0, 0);
      Self.Stack.Clear;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self       : in out Template_Parser;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean)
   is
      use type VSS.IRIs.IRI;
      use type VSS.Strings.Virtual_String;
      use type VSS.XML.Implementation.Template_Programs.Instruction_Kind;

      Attributes_Program :
        VSS.XML.Implementation.Template_Programs.Instruction_Vectors.Vector;
      Condition       : VSS.XML.Implementation.Template_Programs.Instruction;
      Content_Replace : VSS.XML.Implementation.Template_Programs.Instruction;
      Omit_Tag        : VSS.XML.Implementation.Template_Programs.Instruction;
      Repeat          : VSS.XML.Implementation.Template_Programs.Instruction;
      Add_Location    : Boolean := False;

      procedure Parse_Attributes (Text : VSS.Strings.Virtual_String);

      procedure Parse_Condition (Text : VSS.Strings.Virtual_String);

      procedure Parse_Content (Text : VSS.Strings.Virtual_String);

      procedure Parse_Omit_Tag (Text : VSS.Strings.Virtual_String);

      procedure Parse_Repeat (Text : VSS.Strings.Virtual_String);

      procedure Append_Attribute_Expression
        (URI  : VSS.IRIs.IRI;
         Name : VSS.Strings.Virtual_String;
         Path : VSS.String_Vectors.Virtual_String_Vector);

      ---------------------------------
      -- Append_Attribute_Expression --
      ---------------------------------

      procedure Append_Attribute_Expression
        (URI  : VSS.IRIs.IRI;
         Name : VSS.Strings.Virtual_String;
         Path : VSS.String_Vectors.Virtual_String_Vector) is
      begin
         for Instruction of Attributes_Program loop
            if Instruction.Attribute_URI = URI
              and then Instruction.Attribute_Name = Name
            then
               Instruction.Attribute_Path := Path;

               return;
            end if;
         end loop;

         Attributes_Program.Append
           (VSS.XML.Implementation.Template_Programs.Instruction'
              (Kind            =>
                 VSS.XML.Implementation.Template_Programs.Attribute,
               Attribute_URI   => URI,
               Attribute_Name  => Name,
               Attribute_Value => <>,
               Attribute_Path  => Path));

         Add_Location := True;
      end Append_Attribute_Expression;

      ----------------------
      -- Parse_Attributes --
      ----------------------

      procedure Parse_Attributes (Text : VSS.Strings.Virtual_String) is
         Statements : constant VSS.String_Vectors.Virtual_String_Vector :=
           Text.Split (';');

      begin
         for Statement of Statements loop
            declare
               Parts : constant VSS.String_Vectors.Virtual_String_Vector :=
                 Statement.Split (' ');
               Name  : constant VSS.Strings.Virtual_String := Parts (1);
               Path  : constant VSS.String_Vectors.Virtual_String_Vector :=
                 Parts (2).Split ('/');

            begin
               Append_Attribute_Expression (VSS.IRIs.Empty_IRI, Name, Path);
            end;
         end loop;
      end Parse_Attributes;

      ---------------------
      -- Parse_Condition --
      ---------------------

      procedure Parse_Condition (Text : VSS.Strings.Virtual_String) is
         Parts : constant VSS.String_Vectors.Virtual_String_Vector :=
           Text.Split (':');
         Path  : constant VSS.String_Vectors.Virtual_String_Vector :=
           (if Parts.Length = 1
            then Parts (1).Split ('/')
            else Parts (2).Split ('/'));

      begin
         Condition :=
           (Kind           =>
              VSS.XML.Implementation.Template_Programs.Condition,
            Negate         => Parts (1) = "not",
            Condition_Path => Path);
      end Parse_Condition;

      -------------------
      -- Parse_Content --
      -------------------

      procedure Parse_Content (Text : VSS.Strings.Virtual_String) is
         Parts      : constant VSS.String_Vectors.Virtual_String_Vector :=
           Text.Split (' ');
         Format     : constant VSS.Strings.Virtual_String :=
           (if Parts.Length = 1 then text_Keyword else Parts (1));
         Expression : constant VSS.Strings.Virtual_String :=
           (if Parts.Length = 1 then Parts (1) else Parts (2));
         Path       : constant VSS.String_Vectors.Virtual_String_Vector :=
           Expression.Split ('/');

      begin
         if Format /= text_Keyword and Format /= structure_Keyword then
            --  Self.Error.Error ("Unknown format of content");

            return;
         end if;

         Content_Replace :=
           (Kind         => VSS.XML.Implementation.Template_Programs.Content,
            Is_Text      => Format = text_Keyword,
            Content_Path => Path);

         Add_Location := True;
      end Parse_Content;

      --------------------
      -- Parse_Omit_Tag --
      --------------------

      procedure Parse_Omit_Tag (Text : VSS.Strings.Virtual_String) is
      begin
         Omit_Tag :=
           (Kind     => VSS.XML.Implementation.Template_Programs.Omit_Tag,
            Omit_Tag => Text.Is_Empty);
      end Parse_Omit_Tag;

      ------------------
      -- Parse_Repeat --
      ------------------

      procedure Parse_Repeat (Text : VSS.Strings.Virtual_String) is
         Expr : constant VSS.String_Vectors.Virtual_String_Vector :=
           Text.Split (' ');
         Name : constant VSS.Strings.Virtual_String := Expr (1);
         Path : constant VSS.String_Vectors.Virtual_String_Vector :=
           Expr (2).Split ('/');

      begin
         Repeat :=
           (VSS.XML.Implementation.Template_Programs.Repeat, Name, Path);

         Add_Location := True;
      end Parse_Repeat;

      Instructions  : Natural := 0;
      Start_Address : VSS.XML.Implementation.Template_Programs.Address;

   begin
      --  Prepare instruction for each of present attribute from non-TAL
      --  namespace.

      for J in 1 .. Attributes.Get_Length loop
         if Attributes.Get_URI (J) /= VSS.XML.Namespaces.TAL_Namespace then
            Attributes_Program.Append
              (VSS.XML.Implementation.Template_Programs.Instruction'
                 (Kind            =>
                      VSS.XML.Implementation.Template_Programs.Attribute,
                  Attribute_URI   => Attributes.Get_URI (J),
                  Attribute_Name  => Attributes.Get_Name (J),
                  Attribute_Value => Attributes.Get_Value (J),
                  Attribute_Path  => <>));
         end if;
      end loop;

      --  Process attributes from TAL namespace.

      for J in 1 .. Attributes.Get_Length loop
         if Attributes.Get_URI (J) = VSS.XML.Namespaces.TAL_Namespace then
            if Attributes.Get_Name (J) = attributes_Attribute then
               Parse_Attributes (Attributes.Get_Value (J));

            elsif Attributes.Get_Name (J) = condition_Attribute then
               Parse_Condition (Attributes.Get_Value (J));

            elsif Attributes.Get_Name (J) = content_Attribute then
               Parse_Content (Attributes.Get_Value (J));

            elsif Attributes.Get_Name (J) = omit_tag_Attribute then
               Parse_Omit_Tag (Attributes.Get_Value (J));

            elsif Attributes.Get_Name (J) = repeat_Attribute then
               Parse_Repeat (Attributes.Get_Value (J));

            else
               Self.Report_Error ("Unknown TAL attribute", Success);
            end if;
         end if;
      end loop;

      --  Add location information when any instructions required evaluation
      --  will be added for element.

      if Add_Location then
         Self.Program.Append
           (VSS.XML.Implementation.Template_Programs.Instruction'
              (Kind      => VSS.XML.Implementation.Template_Programs.Location,
               System_Id => Self.Locator.Get_System_Id,
               Line      => Self.Locator.Get_Line_Number,
               Column    => Self.Locator.Get_Column_Number));
      end if;

      if Condition.Kind /= VSS.XML.Implementation.Template_Programs.None then
         Self.Program.Append (Condition);
         Instructions := @ + 1;
      end if;

      if Repeat.Kind /= VSS.XML.Implementation.Template_Programs.None then
         Self.Program.Append (Repeat);
         Instructions := @ + 1;
      end if;

      if Content_Replace.Kind
        /= VSS.XML.Implementation.Template_Programs.None
      then
         Self.Program.Append (Content_Replace);
         Instructions := @ + 1;
      end if;

      if Omit_Tag.Kind /= VSS.XML.Implementation.Template_Programs.None then
         Self.Program.Append (Omit_Tag);
         Instructions := @ + 1;
      end if;

      Self.Program.Append
        (VSS.XML.Implementation.Template_Programs.Instruction'
           (VSS.XML.Implementation.Template_Programs.Start_Element,
            URI,
            Name));
      Start_Address := Self.Program.Last_Index;
      Self.Program.Append_Vector (Attributes_Program);

      Self.Stack.Append (Self.Current);
      Self.Current :=
        (Start_Address => Start_Address,
         Instructions  => Instructions);
   end Start_Element;

end VSS.XML.Implementation.Template_Parsers;
