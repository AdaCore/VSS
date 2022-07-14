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
      return Self.Text;
   end Content;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self : Virtual_String_Proxy) return VSS.XML.Templates.Values.Value is
   begin
      return (VSS.XML.Templates.Values.String, Self.Text);
   end Value;

end VSS.XML.Templates.Proxies.Strings;
