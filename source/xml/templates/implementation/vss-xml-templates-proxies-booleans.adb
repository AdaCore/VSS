--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Templates.Proxies.Booleans is

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self : Boolean_Proxy) return VSS.XML.Templates.Values.Value is
   begin
      return (VSS.XML.Templates.Values.Boolean, Self.Value);
   end Value;

end VSS.XML.Templates.Proxies.Booleans;
