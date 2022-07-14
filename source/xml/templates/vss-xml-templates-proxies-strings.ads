--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package VSS.XML.Templates.Proxies.Strings is

   type Virtual_String_Proxy is
     limited new VSS.XML.Templates.Proxies.Abstract_Text_Content_Proxy
       and VSS.XML.Templates.Proxies.Abstract_Value_Proxy with
   record
      Text : VSS.Strings.Virtual_String;
   end record;

   overriding function Content
     (Self : Virtual_String_Proxy) return VSS.Strings.Virtual_String;

   overriding function Value
     (Self : Virtual_String_Proxy) return VSS.XML.Templates.Values.Value;

end VSS.XML.Templates.Proxies.Strings;
