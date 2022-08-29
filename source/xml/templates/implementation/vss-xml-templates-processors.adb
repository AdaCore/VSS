--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.XML.Implementation.Template_Evaluators;
with VSS.XML.Namespaces;

package body VSS.XML.Templates.Processors is

   use type VSS.XML.Content_Handlers.SAX_Content_Handler_Access;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Self  : in out XML_Template_Processor'Class;
      Path  : VSS.String_Vectors.Virtual_String_Vector;
      Proxy : not null VSS.XML.Templates.Proxies.Proxy_Access) is
   begin
      Self.Binded.Bind (Path, Proxy);
   end Bind;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out XML_Template_Processor;
      Text    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      if Self.Depth /= 0 then
         Self.Parser.Characters (Text, Success);

      else
         if Self.Content /= null then
            Self.Content.Characters (Text, Success);
         end if;
      end if;
   end Characters;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document
     (Self    : in out XML_Template_Processor;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Self.Locator := null;
   end End_Document;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self    : in out XML_Template_Processor;
      URI     : VSS.IRIs.IRI;
      Name    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      if Self.Depth /= 0 then
         Self.Depth := @ - 1;

         Self.Parser.End_Element (URI, Name, Success);

         if Self.Depth = 0 then
            Self.Parser.End_Document (Success);

            if Success then
               declare
                  Evaluator :
                    VSS.XML.Implementation.Template_Evaluators
                      .Template_Evaluator;

               begin
                  Evaluator.Content := Self.Content;

                  Evaluator.Evaluate
                    (Self.Binded'Unchecked_Access,
                     Self.Parser.Program,
                     Success);
               end;
            end if;
         end if;

      else
         if Self.Content /= null then
            Self.Content.End_Element (URI, Name, Success);
         end if;
      end if;
   end End_Element;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   overriding procedure Processing_Instruction
     (Self    : in out XML_Template_Processor;
      Target  : VSS.Strings.Virtual_String;
      Data    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      if Self.Content /= null then
         Self.Content.Processing_Instruction (Target, Data, Success);
      end if;
   end Processing_Instruction;

   -------------------------
   -- Set_Content_Handler --
   -------------------------

   procedure Set_Content_Handler
     (Self    : in out XML_Template_Processor;
      Handler : VSS.XML.Content_Handlers.SAX_Content_Handler_Access) is
   begin
      Self.Content := Handler;
   end Set_Content_Handler;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Self    : in out XML_Template_Processor;
      Locator : VSS.XML.Locators.SAX_Locator_Access) is
   begin
      Self.Locator := Locator;
      Self.Parser.Set_Document_Locator (Locator);
   end Set_Document_Locator;

   -----------------------
   -- Set_Error_Handler --
   -----------------------

   procedure Set_Error_Handler
     (Self    : in out XML_Template_Processor;
      Handler : VSS.XML.Error_Handlers.SAX_Error_Handler_Access) is
   begin
      Self.Error := Handler;
   end Set_Error_Handler;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
     (Self    : in out XML_Template_Processor;
      Success : in out Boolean) is
   begin
      Self.Depth := 0;

      if Self.Content /= null then
         Self.Content.Start_Document (Success);
      end if;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self       : in out XML_Template_Processor;
      URI        : VSS.IRIs.IRI;
      Name       : VSS.Strings.Virtual_String;
      Attributes : VSS.XML.Attributes.XML_Attributes'Class;
      Success    : in out Boolean)
   is
      use type VSS.IRIs.IRI;

      Has_TAL : Boolean := False;

   begin
      if Self.Depth /= 0 then
         Self.Depth := @ + 1;
         Self.Parser.Start_Element (URI, Name, Attributes, Success);

      else
         for J in 1 .. Attributes.Get_Length loop
            if Attributes.Get_URI (J) = VSS.XML.Namespaces.TAL_Namespace then
               Has_TAL := True;

               exit;
            end if;
         end loop;

         if Has_TAL then
            Self.Depth := @ + 1;
            Self.Parser.Start_Document (Success);
            Self.Parser.Start_Element (URI, Name, Attributes, Success);

         else
            if Self.Content /= null then
               Self.Content.Start_Element (URI, Name, Attributes, Success);
            end if;
         end if;
      end if;
   end Start_Element;

end VSS.XML.Templates.Processors;
