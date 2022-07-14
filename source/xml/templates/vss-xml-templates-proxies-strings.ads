--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package VSS.XML.Templates.Proxies.Strings is

   type Virtual_String_Proxy is
     limited new VSS.XML.Templates.Proxies.Abstract_Content_Proxy with
   record
      Content : VSS.Strings.Virtual_String;
   end record;

   overriding function Content
     (Self : Virtual_String_Proxy) return VSS.Strings.Virtual_String;

end VSS.XML.Templates.Proxies.Strings;
