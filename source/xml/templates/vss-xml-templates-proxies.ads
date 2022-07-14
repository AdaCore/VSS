--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

with VSS.XML.Event_Vectors;
with VSS.XML.Templates.Values;

package VSS.XML.Templates.Proxies is

   type Abstract_Proxy is limited interface;

   type Proxy_Access is
     access all VSS.XML.Templates.Proxies.Abstract_Proxy'Class;

   type Abstract_Iterable_Iterator is limited interface and Abstract_Proxy;

   function Next
     (Self : in out Abstract_Iterable_Iterator) return Boolean is abstract;

   function Element
     (Self : in out Abstract_Iterable_Iterator)
      return Abstract_Proxy'Class is abstract;

   type Abstract_Iterable_Proxy is limited interface and Abstract_Proxy;

   function Iterator
     (Self : in out Abstract_Iterable_Proxy)
      return Abstract_Iterable_Iterator'Class is abstract;

   function Is_Empty
     (Self : Abstract_Iterable_Proxy) return Boolean is abstract;

   type Abstract_Composite_Proxy is limited interface and Abstract_Proxy;

   function Component
     (Self : in out Abstract_Composite_Proxy;
      Name : VSS.Strings.Virtual_String)
      return Abstract_Proxy'Class is abstract;

   type Abstract_Text_Content_Proxy is limited interface and Abstract_Proxy;

   function Content
     (Self : Abstract_Text_Content_Proxy)
      return VSS.Strings.Virtual_String is abstract;

   type Abstract_Structure_Content_Proxy is
     limited interface and Abstract_Proxy;

   function Content
     (Self : in out Abstract_Structure_Content_Proxy)
      return VSS.XML.Event_Vectors.Vector is abstract;

   type Abstract_Value_Proxy is limited interface and Abstract_Proxy;

   function Value
     (Self : Abstract_Value_Proxy)
      return VSS.XML.Templates.Values.Value is abstract;

   type Error_Proxy is limited new Abstract_Proxy with record
      Message : VSS.Strings.Virtual_String;
   end record;

end VSS.XML.Templates.Proxies;
