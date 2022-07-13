--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.IRIs;
with VSS.Strings.Conversions;
with VSS.XML.Implementation.XmlAda_Attributes;
with VSS.XML.Implementation.XmlAda_Parse_Errors;

package body VSS.XML.XmlAda_Readers is

   use type VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
   use type VSS.XML.Error_Handlers.SAX_Error_Handler_Access;
   use type VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out XmlAda_Dispatcher;
      Ch   : Unicode.CES.Byte_Sequence) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.Characters
           (VSS.Strings.Conversions.To_Virtual_String (Ch), Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Characters;

   -------------
   -- Comment --
   -------------

   overriding procedure Comment
     (Self : in out XmlAda_Dispatcher;
      Ch   : Unicode.CES.Byte_Sequence) is
   begin
      pragma Assert (Self.Success);

      if Self.Lexical /= null then
         Self.Lexical.Comment
           (VSS.Strings.Conversions.To_Virtual_String (Ch), Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Comment;

   ---------------
   -- End_Cdata --
   ---------------

   overriding procedure End_Cdata (Self : in out XmlAda_Dispatcher) is
   begin
      pragma Assert (Self.Success);

      if Self.Lexical /= null then
         Self.Lexical.End_CDATA (Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end End_Cdata;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document (Self : in out XmlAda_Dispatcher) is
   begin
      if Self.Content /= null then
         Self.Content.End_Document (Self.Success);
      end if;
   end End_Document;

   -------------
   -- End_DTD --
   -------------

   overriding procedure End_DTD (Self : in out XmlAda_Dispatcher) is
   begin
      pragma Assert (Self.Success);

      if Self.Lexical /= null then
         Self.Lexical.End_DTD (Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end End_DTD;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self       : in out XmlAda_Dispatcher;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.End_Element
           (URI     =>
              VSS.IRIs.To_IRI
                (VSS.Strings.Conversions.To_Virtual_String
                     (Sax.Symbols.Get (Sax.Utils.Get_URI (NS)).all)),
            Name    =>
              VSS.Strings.Conversions.To_Virtual_String
                (Sax.Symbols.Get (Local_Name).all),
            Success => Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end End_Element;

   ----------------
   -- End_Entity --
   ----------------

   overriding procedure End_Entity
     (Self : in out XmlAda_Dispatcher;
      Name : Sax.Symbols.Symbol) is
   begin
      pragma Assert (Self.Success);

      if Self.Lexical /= null then
         Self.Lexical.End_Entity
           (VSS.Strings.Conversions.To_Virtual_String
              (Sax.Symbols.Get (Name).all),
            Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end End_Entity;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   overriding procedure End_Prefix_Mapping
     (Self   : in out XmlAda_Dispatcher;
      Prefix : Sax.Symbols.Symbol) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.End_Prefix_Mapping
           (VSS.Strings.Conversions.To_Virtual_String
              (Sax.Symbols.Get (Prefix).all),
            Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end End_Prefix_Mapping;

   -----------
   -- Error --
   -----------

   overriding procedure Error
     (Self   : in out XmlAda_Dispatcher;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      pragma Assert (Self.Success);

      if Self.Error /= null then
         declare
            Error : constant
              VSS.XML.Implementation.XmlAda_Parse_Errors.Parse_Error :=
                (Error => Except'Unchecked_Access);

         begin
            Self.Error.Error (Error, Self.Success);
         end;
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   overriding procedure Fatal_Error
     (Self   : in out XmlAda_Dispatcher;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      pragma Assert (Self.Success);

      if Self.Error /= null then
         declare
            Error : constant
              VSS.XML.Implementation.XmlAda_Parse_Errors.Parse_Error :=
                (Error => Except'Unchecked_Access);

         begin
            Self.Error.Fatal_Error (Error, Self.Success);
         end;
      end if;

      --  XmlAda's parser doesn't expect normal return from this subprogram,
      --  thus raise XML_Fatal_Error to catch it inside Parse subprogram.

      Self.Success := False;

      raise Sax.Readers.XML_Fatal_Error;
   end Fatal_Error;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   overriding procedure Ignorable_Whitespace
     (Self : in out XmlAda_Dispatcher;
      Ch   : Unicode.CES.Byte_Sequence) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.Ignorable_Whitespace
           (VSS.Strings.Conversions.To_Virtual_String (Ch), Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Ignorable_Whitespace;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self  : in out XmlAda_Reader'Class;
      Input : in out Input_Sources.Input_Source'Class) is
   begin
      Self.Dispatcher.Success := True;
      Self.Dispatcher.Parse (Input);

   exception
      when Sax.Readers.XML_Fatal_Error =>
         --  This exception is used to stop parsing of the document for any
         --  reason (fatal error has been found in the document or processing
         --  is terminated by the application).

         pragma Assert (not Self.Dispatcher.Success);
   end Parse;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   overriding procedure Processing_Instruction
     (Self   : in out XmlAda_Dispatcher;
      Target : Unicode.CES.Byte_Sequence;
      Data   : Unicode.CES.Byte_Sequence) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.Processing_Instruction
           (Target  => VSS.Strings.Conversions.To_Virtual_String (Target),
            Data    => VSS.Strings.Conversions.To_Virtual_String (Target),
            Success => Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Processing_Instruction;

   -------------------------
   -- Set_Content_Handler --
   -------------------------

   overriding procedure Set_Content_Handler
     (Self    : in out XmlAda_Reader;
      Handler : VSS.XML.Content_Handlers.SAX_Content_Handler_Access) is
   begin
      Self.Dispatcher.Content := Handler;
   end Set_Content_Handler;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Self : in out XmlAda_Dispatcher;
      Loc  : in out Sax.Locators.Locator) is
   begin
      pragma Assert (Self.Success);

      Self.Locator.Locator := Loc;

      if Self.Content /= null then
         Self.Content.Set_Document_Locator (Self.Locator'Unchecked_Access);
      end if;
   end Set_Document_Locator;

   -----------------------
   -- Set_Error_Handler --
   -----------------------

   overriding procedure Set_Error_Handler
     (Self    : in out XmlAda_Reader;
      Handler : VSS.XML.Error_Handlers.SAX_Error_Handler_Access) is
   begin
      Self.Dispatcher.Error := Handler;
   end Set_Error_Handler;

   -------------------------
   -- Set_Lexical_Handler --
   -------------------------

   overriding procedure Set_Lexical_Handler
     (Self    : in out XmlAda_Reader;
      Handler : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access) is
   begin
      Self.Dispatcher.Lexical := Handler;
   end Set_Lexical_Handler;

   --------------------
   -- Skipped_Entity --
   --------------------

   overriding procedure Skipped_Entity
     (Self : in out XmlAda_Dispatcher;
      Name : Sax.Symbols.Symbol) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.Skipped_Entity
           (VSS.Strings.Conversions.To_Virtual_String
              (Sax.Symbols.Get (Name).all),
            Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Skipped_Entity;

   -----------------
   -- Start_Cdata --
   -----------------

   overriding procedure Start_Cdata (Self : in out XmlAda_Dispatcher) is
   begin
      pragma Assert (Self.Success);

      if Self.Lexical /= null then
         Self.Lexical.Start_CDATA (Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Start_Cdata;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document (Self : in out XmlAda_Dispatcher) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.Start_Document (Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Start_Document;

   ---------------
   -- Start_DTD --
   ---------------

   overriding procedure Start_DTD
     (Self      : in out XmlAda_Dispatcher;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence) is
   begin
      pragma Assert (Self.Success);

      if Self.Lexical /= null then
         Self.Lexical.Start_DTD
           (Name      => VSS.Strings.Conversions.To_Virtual_String (Name),
            Public_Id => VSS.Strings.Conversions.To_Virtual_String (Public_Id),
            System_Id => VSS.Strings.Conversions.To_Virtual_String (System_Id),
            Success   => Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Start_DTD;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self       : in out XmlAda_Dispatcher;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.Start_Element
           (URI        =>
              VSS.IRIs.To_IRI
                (VSS.Strings.Conversions.To_Virtual_String
                     (Sax.Symbols.Get (Sax.Utils.Get_URI (NS)).all)),
            Name       =>
              VSS.Strings.Conversions.To_Virtual_String
          (Sax.Symbols.Get (Local_Name).all),
            Attributes =>
              VSS.XML.Implementation.XmlAda_Attributes.XmlAda_Attributes'
                (Attributes => Atts),
            Success    => Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Start_Element;

   ------------------
   -- Start_Entity --
   ------------------

   overriding procedure Start_Entity
     (Self : in out XmlAda_Dispatcher;
      Name : Sax.Symbols.Symbol) is
   begin
      pragma Assert (Self.Success);

      if Self.Lexical /= null then
         Self.Lexical.Start_Entity
           (VSS.Strings.Conversions.To_Virtual_String
              (Sax.Symbols.Get (Name).all),
            Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Start_Entity;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   overriding procedure Start_Prefix_Mapping
     (Self   : in out XmlAda_Dispatcher;
      Prefix : Sax.Symbols.Symbol;
      URI    : Sax.Symbols.Symbol) is
   begin
      pragma Assert (Self.Success);

      if Self.Content /= null then
         Self.Content.Start_Prefix_Mapping
           (VSS.Strings.Conversions.To_Virtual_String
              (Sax.Symbols.Get (Prefix).all),
            VSS.IRIs.To_IRI
              (VSS.Strings.Conversions.To_Virtual_String
                 (Sax.Symbols.Get (URI).all)),
            Self.Success);
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Start_Prefix_Mapping;

   -------------
   -- Warning --
   -------------

   overriding procedure Warning
     (Self   : in out XmlAda_Dispatcher;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      pragma Assert (Self.Success);

      if Self.Error /= null then
         declare
            Error : constant
              VSS.XML.Implementation.XmlAda_Parse_Errors.Parse_Error :=
                (Error => Except'Unchecked_Access);

         begin
            Self.Error.Warning (Error, Self.Success);
         end;
      end if;

      if not Self.Success then
         raise Sax.Readers.XML_Fatal_Error;
      end if;
   end Warning;

end VSS.XML.XmlAda_Readers;
