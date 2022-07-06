--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;

with VSS.Strings.Hash;
with VSS.String_Vectors;
with VSS.XML.Templates.Proxies;
with VSS.XML.Templates.Values;

package VSS.XML.Implementation.Template_Namespaces is

   package Name_Item_Maps is
     new Ada.Containers.Hashed_Maps
       (VSS.Strings.Virtual_String,
        VSS.XML.Templates.Proxy_Access,
        VSS.Strings.Hash,
        VSS.Strings."=",
        VSS.XML.Templates."=");

   type Iterable_Iterator_Access is
     access all VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class;

   type Namespace;  --  is tagged;

   type Namespace_Access is access all Namespace;  --  'Class;

   type Namespace is
     limited new VSS.XML.Templates.Proxies.Abstract_Proxy with record
      Enclosing : Namespace_Access;
      Items     : Name_Item_Maps.Map;
   end record;

   function Resolve_Iterable
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return Iterable_Iterator_Access
     with Pre => not Path.Is_Empty;

   function Resolve_Content
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.Strings.Virtual_String
     with Pre => not Path.Is_Empty;

   function Resolve_Value
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.XML.Templates.Values.Value
     with Pre => not Path.Is_Empty;

   procedure Bind
     (Self : in out Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector;
      Item : not null VSS.XML.Templates.Proxy_Access)
     with Pre => not Path.Is_Empty;

   procedure Bind
     (Self : in out Namespace'Class;
      Name : VSS.Strings.Virtual_String;
      Item : not null VSS.XML.Templates.Proxy_Access)
     with Pre => not Name.Is_Empty;

end VSS.XML.Implementation.Template_Namespaces;
