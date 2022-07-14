--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Finalization;

with VSS.Strings.Hash;
with VSS.String_Vectors;
with VSS.XML.Event_Vectors;
with VSS.XML.Templates.Proxies;
with VSS.XML.Templates.Values;

package VSS.XML.Implementation.Template_Namespaces is

   type Error_Handler is limited interface;

   procedure Report_Error
     (Self    : in out Error_Handler;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean) is abstract;

   package Name_Item_Maps is
     new Ada.Containers.Hashed_Maps
       (VSS.Strings.Virtual_String,
        VSS.XML.Templates.Proxies.Proxy_Access,
        VSS.Strings.Hash,
        VSS.Strings."=",
        VSS.XML.Templates.Proxies."=");

   type Iterable_Iterator_Access is
     access all VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class;

   type Namespace is tagged;

   type Namespace_Access is access all Namespace'Class;

   type Namespace is
     limited new Ada.Finalization.Limited_Controlled
       and VSS.XML.Templates.Proxies.Abstract_Proxy with
   record
      Enclosing : Namespace_Access;
      Items     : Name_Item_Maps.Map;
   end record;

   procedure Resolve
     (Self      : Namespace'Class;
      Path      : VSS.String_Vectors.Virtual_String_Vector;
      Proxy     : out VSS.XML.Templates.Proxies.Proxy_Access;
      Remaining : out VSS.String_Vectors.Virtual_String_Vector);
   --  Attempt to resolve proxy till it's binding point. Returned proxy is
   --  managed by the namespace.

   function Resolve_Iterable
     (Self    : Namespace'Class;
      Path    : VSS.String_Vectors.Virtual_String_Vector;
      Error   : in out Error_Handler'Class;
      Success : in out Boolean) return Iterable_Iterator_Access
     with Pre => not Path.Is_Empty;

   function Resolve_Text_Content
     (Self    : Namespace'Class;
      Path    : VSS.String_Vectors.Virtual_String_Vector;
      Error   : in out Error_Handler'Class;
      Success : in out Boolean)
      return VSS.Strings.Virtual_String
     with Pre => not Path.Is_Empty;

   function Resolve_Structure_Content
     (Self    : Namespace'Class;
      Path    : VSS.String_Vectors.Virtual_String_Vector;
      Error   : in out Error_Handler'Class;
      Success : in out Boolean) return VSS.XML.Event_Vectors.Vector
     with Pre => not Path.Is_Empty;

   function Resolve_Value
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.XML.Templates.Values.Value
     with Pre => not Path.Is_Empty;

   function Resolve_Boolean_Value
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.XML.Templates.Values.Value
     with Pre => not Path.Is_Empty;

   procedure Bind
     (Self : in out Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector;
      Item : not null VSS.XML.Templates.Proxies.Proxy_Access)
     with Pre => not Path.Is_Empty;

   procedure Bind
     (Self : in out Namespace'Class;
      Name : VSS.Strings.Virtual_String;
      Item : not null VSS.XML.Templates.Proxies.Proxy_Access)
     with Pre => not Name.Is_Empty;

   overriding procedure Finalize (Self : in out Namespace);

end VSS.XML.Implementation.Template_Namespaces;
