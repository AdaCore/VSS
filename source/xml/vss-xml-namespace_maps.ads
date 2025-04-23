--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides support for mapping XML namespace prefixes to XML
--  namespace URIs and vice versa.

private with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
private with Ada.Finalization;

with VSS.IRIs;
private with VSS.IRIs.Hash;
with VSS.Strings;
private with VSS.Strings.Hash;

package VSS.XML.Namespace_Maps is

   type XML_Namespace_Map is tagged limited private;

   procedure Start_Prefix_Mapping
     (Self   : in out XML_Namespace_Map'Class;
      Prefix : VSS.Strings.Virtual_String;
      URI    : VSS.IRIs.IRI);
   --  Add new mapping. Mapping is going to be active after the call of
   --  Start_Element.

   procedure Start_Element (Self : in out XML_Namespace_Map'Class);
   --  Push state to activate mappings of element.

   procedure End_Element (Self : in out XML_Namespace_Map'Class);
   --  Pop state to return to mappings defined for parent element.

   function Prefix
     (Self : XML_Namespace_Map'Class;
      URI  : VSS.IRIs.IRI) return VSS.Strings.Virtual_String;
   --  Return prefix mapped to given URI for current element.

   function URI
     (Self   : XML_Namespace_Map'Class;
      Prefix : VSS.Strings.Virtual_String) return VSS.IRIs.IRI;
   --  Return URI mapped to given prefix for current element.

private

   type Mapping is record
      Prefix : VSS.Strings.Virtual_String;
      URI    : VSS.IRIs.IRI;
   end record;

   package Mapping_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Mapping,
        "="          => "=");

   package Prefix_URI_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => VSS.Strings.Virtual_String,
        Element_Type    => VSS.IRIs.IRI,
        Hash            => VSS.Strings.Hash,
        Equivalent_Keys => VSS.Strings."=",
        "="             => VSS.IRIs."=");

   package URI_Prefix_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => VSS.IRIs.IRI,
        Element_Type    => VSS.Strings.Virtual_String,
        Hash            => VSS.IRIs.Hash,
        Equivalent_Keys => VSS.IRIs."=",
        "="             => VSS.Strings."=");

   type Map is record
      To_Prefix : URI_Prefix_Maps.Map;
      To_URI    : Prefix_URI_Maps.Map;
      Depth     : Natural := 0;
   end record;

   package Map_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Map,
        "="          => "=");

   type XML_Namespace_Map is
     new Ada.Finalization.Limited_Controlled with record
      Stack    : Map_Vectors.Vector;
      Active   : Map;
      Mappings : Mapping_Vectors.Vector;
   end record;

   overriding procedure Initialize (Self : in out XML_Namespace_Map);

end VSS.XML.Namespace_Maps;
