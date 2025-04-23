--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.XML.Namespaces;

package body VSS.XML.Namespace_Maps is

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element (Self : in out XML_Namespace_Map'Class) is
   begin
      Self.Active.Depth := @ - 1;

      if Self.Active.Depth = 0 then
         Self.Active := Self.Stack.Last_Element;
         Self.Stack.Delete_Last;
      end if;
   end End_Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out XML_Namespace_Map) is
      XML_Prefix   : constant VSS.Strings.Virtual_String := "xml";
      XMLNS_Prefix : constant VSS.Strings.Virtual_String := "xmlns";

   begin
      Self.Active.Depth := 0;
      Self.Active.To_Prefix.Insert
        (VSS.XML.Namespaces.XML_Namespace, XML_Prefix);
      Self.Active.To_URI.Insert (XML_Prefix, VSS.XML.Namespaces.XML_Namespace);
      Self.Active.To_Prefix.Insert
        (VSS.XML.Namespaces.XMLNS_Namespace, XMLNS_Prefix);
      Self.Active.To_URI.Insert
        (XMLNS_Prefix, VSS.XML.Namespaces.XMLNS_Namespace);
   end Initialize;

   ------------
   -- Prefix --
   ------------

   function Prefix
     (Self : XML_Namespace_Map'Class;
      URI  : VSS.IRIs.IRI) return VSS.Strings.Virtual_String is
   begin
      return Self.Active.To_Prefix (URI);
   end Prefix;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element (Self : in out XML_Namespace_Map'Class) is
   begin
      if Self.Mappings.Is_Empty then
         Self.Active.Depth := @ + 1;

      else
         Self.Stack.Append (Self.Active);

         Self.Active.Depth := 1;

         for Mapping of Self.Mappings loop
            Self.Active.To_Prefix.Include (Mapping.URI, Mapping.Prefix);
            Self.Active.To_URI.Include (Mapping.Prefix, Mapping.URI);
         end loop;

         Self.Mappings.Clear;
      end if;
   end Start_Element;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Self   : in out XML_Namespace_Map'Class;
      Prefix : VSS.Strings.Virtual_String;
      URI    : VSS.IRIs.IRI) is
   begin
      Self.Mappings.Append (Mapping'(Prefix => Prefix, URI => URI));
   end Start_Prefix_Mapping;

   ---------
   -- URI --
   ---------

   function URI
     (Self   : XML_Namespace_Map'Class;
      Prefix : VSS.Strings.Virtual_String) return VSS.IRIs.IRI is
   begin
      return Self.Active.To_URI (Prefix);
   end URI;

end VSS.XML.Namespace_Maps;
