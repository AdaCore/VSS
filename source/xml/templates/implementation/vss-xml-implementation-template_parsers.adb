--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off);
pragma Ada_2020;
pragma Ada_2022;
pragma Warnings (On);

with VSS.String_Vectors;
with VSS.XML.Namespaces;

package body VSS.XML.Implementation.Template_Parsers is

   attributes_Attribute : constant VSS.Strings.Virtual_String := "attributes";
   repeat_Attribute     : constant VSS.Strings.Virtual_String := "repeat";
   content_Attribute    : constant VSS.Strings.Virtual_String := "content";

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

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Self    : in out Template_Parser;
      Locator : VSS.XML.Locators.SAX_Locator_Access;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

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
      pragma Unreferenced (Success);

      use type VSS.IRIs.IRI;
      use type VSS.Strings.Virtual_String;
      use type VSS.XML.Implementation.Template_Programs.Instruction_Kind;

      Attributes_Program :
        VSS.XML.Implementation.Template_Programs.Instruction_Vectors.Vector;
      Content_Replace : VSS.XML.Implementation.Template_Programs.Instruction;
      Repeat          : VSS.XML.Implementation.Template_Programs.Instruction;

      procedure Parse_Attributes (Text : VSS.Strings.Virtual_String);

      procedure Parse_Content (Text : VSS.Strings.Virtual_String);

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

      -------------------
      -- Parse_Content --
      -------------------

      procedure Parse_Content (Text : VSS.Strings.Virtual_String) is
         Parts      : constant VSS.String_Vectors.Virtual_String_Vector :=
           Text.Split (' ');
         Format     : constant VSS.Strings.Virtual_String :=
           (if Parts.Length = 1 then "text" else Parts (1));
         Expression : constant VSS.Strings.Virtual_String :=
           (if Parts.Length = 1 then Parts (1) else Parts (2));
         Path       : constant VSS.String_Vectors.Virtual_String_Vector :=
           Expression.Split ('/');

      begin
         if Format /= "text" and Format /= "structure" then
            --  Self.Error.Error ("Unknown format of content");

            return;
         end if;

         Content_Replace :=
           (Kind         => VSS.XML.Implementation.Template_Programs.Content,
            Is_Text      => Format = "text",
            Content_Path => Path);
      end Parse_Content;

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

            elsif Attributes.Get_Name (J) = content_Attribute then
               Parse_Content (Attributes.Get_Value (J));

            elsif Attributes.Get_Name (J) = repeat_Attribute then
               Parse_Repeat (Attributes.Get_Value (J));

            else
               --  Self.Error.Error ("Unknown TAL attribute.

               null;
            end if;
         end if;
      end loop;

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
