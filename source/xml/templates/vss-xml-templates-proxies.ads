--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.XML.Templates.Proxies is

   type Abstract_Proxy is limited interface;

   type Abstract_Iterable_Iterator is limited interface;

   function Next
     (Self : in out Abstract_Iterable_Iterator) return Boolean is abstract;

   type Abstract_Iterable_Proxy is limited interface and Abstract_Proxy;

   function Iterator
     (Self : in out Abstract_Iterable_Proxy)
      return Abstract_Iterable_Iterator'Class is abstract;

   type Abstract_Content_Proxy is limited interface and Abstract_Proxy;

   function Content
     (Self : Abstract_Content_Proxy;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.Strings.Virtual_String is abstract;

   --  function Content
   --    (Self : in out Abstract_Content_Proxy)
   --     return VSS.XML.Events.Sequence;

   type Abstract_Value_Proxy is limited interface and Abstract_Proxy;

end VSS.XML.Templates.Proxies;
