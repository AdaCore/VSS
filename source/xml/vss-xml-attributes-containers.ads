--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides container for set of values of XML attributes to be
--  used by applications.

private with Ada.Containers.Vectors;

package VSS.XML.Attributes.Containers is

   pragma Preelaborate;

   type Attributes is
     new VSS.XML.Attributes.XML_Attributes with private;

   procedure Clear (Self : in out Attributes'Class);

   procedure Insert
     (Self  : in out Attributes'Class;
      URI   : VSS.IRIs.IRI;
      Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String);

private

   type Attribute is record
      URI   : VSS.IRIs.IRI;
      Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String;
   end record;

   package Attribute_Vectors is
     new Ada.Containers.Vectors (Positive, Attribute);

   type Attributes is
     new VSS.XML.Attributes.XML_Attributes with record
      Container : Attribute_Vectors.Vector;
   end record;

   overriding function Get_Length (Self : Attributes) return Natural;

   overriding function Get_URI
     (Self  : Attributes;
      Index : Positive) return VSS.IRIs.IRI;

   overriding function Get_Name
     (Self  : Attributes;
      Index : Positive) return VSS.Strings.Virtual_String;

   overriding function Get_Value
     (Self  : Attributes;
      Index : Positive) return VSS.Strings.Virtual_String;

end VSS.XML.Attributes.Containers;
