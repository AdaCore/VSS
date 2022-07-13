--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.IRIs;
with VSS.Strings.Texts;
with VSS.XML.Implementation.Matreshka_Attributes;
with VSS.XML.Implementation.Parse_Errors;

package body VSS.XML.Matreshka_Readers is

   use type VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
   use type VSS.XML.Error_Handlers.SAX_Error_Handler_Access;
   use type VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access;

   function To_IRI
     (Item : League.Strings.Universal_String) return VSS.IRIs.IRI;

   function To_Virtual_String
     (Item : League.Strings.Universal_String)
      return VSS.Strings.Virtual_String;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out Matreshka_Dispatcher;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.Characters (To_Virtual_String (Text), Success);
      end if;
   end Characters;

   -------------
   -- Comment --
   -------------

   overriding procedure Comment
     (Self    : in out Matreshka_Dispatcher;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Lexical /= null then
         Self.Lexical.Comment (To_Virtual_String (Text), Success);
      end if;
   end Comment;

   ---------------
   -- End_CDATA --
   ---------------

   overriding procedure End_CDATA
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean) is
   begin
      if Self.Lexical /= null then
         Self.Lexical.End_CDATA (Success);
      end if;
   end End_CDATA;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.End_Document (Success);
      end if;
   end End_Document;

   -------------
   -- End_DTD --
   -------------

   overriding procedure End_DTD
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean) is
   begin
      if Self.Lexical /= null then
         Self.Lexical.End_DTD (Success);
      end if;
   end End_DTD;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self           : in out Matreshka_Dispatcher;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.End_Element
           (To_IRI (Namespace_URI),
            To_Virtual_String (Local_Name),
            Success);
      end if;
   end End_Element;

   ----------------
   -- End_Entity --
   ----------------

   overriding procedure End_Entity
     (Self    : in out Matreshka_Dispatcher;
      Name    : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Lexical /= null then
         Self.Lexical.End_Entity (To_Virtual_String (Name), Success);
      end if;
   end End_Entity;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   overriding procedure End_Prefix_Mapping
     (Self    : in out Matreshka_Dispatcher;
      Prefix  : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.End_Prefix_Mapping (To_Virtual_String (Prefix), Success);
      end if;
   end End_Prefix_Mapping;

   -----------
   -- Error --
   -----------

   overriding procedure Error
     (Self       : in out Matreshka_Dispatcher;
      Occurrence : Standard.XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
      Success    : in out Boolean)
   is
      Error :
        constant VSS.XML.Implementation.Parse_Errors.Parse_Error_Location :=
          (Public_Id => To_Virtual_String (Occurrence.Public_Id),
           System_Id => To_Virtual_String (Occurrence.System_Id),
           Line      => VSS.Strings.Texts.Line_Count (Occurrence.Line),
           Column    => VSS.Strings.Character_Count (Occurrence.Column),
           Message   => To_Virtual_String (Occurrence.Message));

   begin
      if Self.Error /= null then
         Self.Error.Error (Error, Success);
      end if;
   end Error;

   ------------------
   -- Error_String --
   ------------------

   overriding function Error_String
     (Self : Matreshka_Dispatcher) return League.Strings.Universal_String is
   begin
      raise Program_Error;
      --  XXX Not yet implemented in VSS.SAX API.
      return League.Strings.Empty_Universal_String;
   end Error_String;

   -----------------
   -- Fatal_Error --
   -----------------

   overriding procedure Fatal_Error
     (Self       : in out Matreshka_Dispatcher;
      Occurrence : Standard.XML.SAX.Parse_Exceptions.SAX_Parse_Exception)
   is
      Error   :
        constant VSS.XML.Implementation.Parse_Errors.Parse_Error_Location :=
          (Public_Id => To_Virtual_String (Occurrence.Public_Id),
           System_Id => To_Virtual_String (Occurrence.System_Id),
           Line      => VSS.Strings.Texts.Line_Count (Occurrence.Line),
           Column    => VSS.Strings.Character_Count (Occurrence.Column),
           Message   => To_Virtual_String (Occurrence.Message));
      Success : Boolean := True;

   begin
      if Self.Error /= null then
         Self.Error.Fatal_Error (Error, Success);
      end if;
   end Fatal_Error;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   overriding procedure Ignorable_Whitespace
     (Self    : in out Matreshka_Dispatcher;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.Ignorable_Whitespace (To_Virtual_String (Text), Success);
      end if;
   end Ignorable_Whitespace;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self  : in out Matreshka_Reader'Class;
      Input : in out Standard.XML.SAX.Input_Sources.SAX_Input_Source'Class) is
   begin
      Self.Reader.Set_Content_Handler (Self.Dispatcher'Unchecked_Access);
      Self.Reader.Set_Error_Handler (Self.Dispatcher'Unchecked_Access);
      Self.Reader.Set_Lexical_Handler (Self.Dispatcher'Unchecked_Access);

      Self.Reader.Parse (Input'Access);
   end Parse;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   overriding procedure Processing_Instruction
     (Self    : in out Matreshka_Dispatcher;
      Target  : League.Strings.Universal_String;
      Data    : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.Processing_Instruction
           (To_Virtual_String (Target),
            To_Virtual_String (Data),
            Success);
      end if;
   end Processing_Instruction;

   -------------------------
   -- Set_Content_Handler --
   -------------------------

   overriding procedure Set_Content_Handler
     (Self    : in out Matreshka_Reader;
      Handler : VSS.XML.Content_Handlers.SAX_Content_Handler_Access) is
   begin
      Self.Dispatcher.Content := Handler;
   end Set_Content_Handler;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Self    : in out Matreshka_Dispatcher;
      Locator : Standard.XML.SAX.Locators.SAX_Locator) is
   begin
      Self.Locator.Locator := Locator;

      if Self.Content /= null then
         Self.Content.Set_Document_Locator
           (Self.Locator'Unchecked_Access);
      end if;
   end Set_Document_Locator;

   -----------------------
   -- Set_Error_Handler --
   -----------------------

   overriding procedure Set_Error_Handler
     (Self    : in out Matreshka_Reader;
      Handler : VSS.XML.Error_Handlers.SAX_Error_Handler_Access) is
   begin
      Self.Dispatcher.Error := Handler;
   end Set_Error_Handler;

   -------------------------
   -- Set_Lexical_Handler --
   -------------------------

   overriding procedure Set_Lexical_Handler
     (Self    : in out Matreshka_Reader;
      Handler : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access) is
   begin
      Self.Dispatcher.Lexical := Handler;
   end Set_Lexical_Handler;

   --------------------
   -- Skipped_Entity --
   --------------------

   overriding procedure Skipped_Entity
     (Self    : in out Matreshka_Dispatcher;
      Name    : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.Skipped_Entity (To_Virtual_String (Name), Success);
      end if;
   end Skipped_Entity;

   -----------------
   -- Start_CDATA --
   -----------------

   overriding procedure Start_CDATA
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean) is
   begin
      if Self.Lexical /= null then
         Self.Lexical.Start_CDATA (Success);
      end if;
   end Start_CDATA;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
     (Self    : in out Matreshka_Dispatcher;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.Start_Document (Success);
      end if;
   end Start_Document;

   ---------------
   -- Start_DTD --
   ---------------

   overriding procedure Start_DTD
     (Self      : in out Matreshka_Dispatcher;
      Name      : League.Strings.Universal_String;
      Public_Id : League.Strings.Universal_String;
      System_Id : League.Strings.Universal_String;
      Success   : in out Boolean) is
   begin
      if Self.Lexical /= null then
         Self.Lexical.Start_DTD
           (To_Virtual_String (Name),
            To_Virtual_String (Public_Id),
            To_Virtual_String (System_Id),
            Success);
      end if;
   end Start_DTD;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self           : in out Matreshka_Dispatcher;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : Standard.XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean)
   is
      Attrs : constant
        VSS.XML.Implementation.Matreshka_Attributes.Matreshka_Attributes
          := (Attributes => Attributes);

   begin
      if Self.Content /= null then
         Self.Content.Start_Element
           (To_IRI (Namespace_URI),
            To_Virtual_String (Local_Name),
            Attrs,
            Success);
      end if;
   end Start_Element;

   ------------------
   -- Start_Entity --
   ------------------

   overriding procedure Start_Entity
     (Self    : in out Matreshka_Dispatcher;
      Name    : League.Strings.Universal_String;
      Success : in out Boolean) is
   begin
      if Self.Lexical /= null then
         Self.Lexical.Start_Entity (To_Virtual_String (Name), Success);
      end if;
   end Start_Entity;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   overriding procedure Start_Prefix_Mapping
     (Self          : in out Matreshka_Dispatcher;
      Prefix        : League.Strings.Universal_String;
      Namespace_URI : League.Strings.Universal_String;
      Success       : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.Start_Prefix_Mapping
           (To_Virtual_String (Prefix), To_IRI (Namespace_URI), Success);
      end if;
   end Start_Prefix_Mapping;

   ------------
   -- To_IRI --
   ------------

   function To_IRI
     (Item : League.Strings.Universal_String) return VSS.IRIs.IRI is
   begin
      return
        VSS.IRIs.To_IRI
          (VSS.Strings.To_Virtual_String (Item.To_Wide_Wide_String));
   end To_IRI;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : League.Strings.Universal_String)
      return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.To_Virtual_String (Item.To_Wide_Wide_String);
   end To_Virtual_String;

   -------------
   -- Warning --
   -------------

   overriding procedure Warning
     (Self       : in out Matreshka_Dispatcher;
      Occurrence : Standard.XML.SAX.Parse_Exceptions.SAX_Parse_Exception;
      Success    : in out Boolean)
   is
      Error :
        constant VSS.XML.Implementation.Parse_Errors.Parse_Error_Location :=
          (Public_Id => To_Virtual_String (Occurrence.Public_Id),
           System_Id => To_Virtual_String (Occurrence.System_Id),
           Line      => VSS.Strings.Texts.Line_Count (Occurrence.Line),
           Column    => VSS.Strings.Character_Count (Occurrence.Column),
           Message   => To_Virtual_String (Occurrence.Message));

   begin
      if Self.Error /= null then
         Self.Error.Warning (Error, Success);
      end if;
   end Warning;

end VSS.XML.Matreshka_Readers;
