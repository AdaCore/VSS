--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Templates.Proxies.Strings is

   -------------
   -- Content --
   -------------

   overriding function Content
     (Self : Virtual_String_Proxy) return VSS.Strings.Virtual_String is
   begin
      return Self.Content;
   end Content;

end VSS.XML.Templates.Proxies.Strings;
