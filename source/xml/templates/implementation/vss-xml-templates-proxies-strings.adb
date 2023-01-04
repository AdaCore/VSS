--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Templates.Proxies.Strings is

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self : Virtual_String_Proxy) return VSS.XML.Templates.Values.Value is
   begin
      return (VSS.XML.Templates.Values.String, Self.Text);
   end Value;

end VSS.XML.Templates.Proxies.Strings;
