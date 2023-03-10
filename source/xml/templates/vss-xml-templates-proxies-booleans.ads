--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package VSS.XML.Templates.Proxies.Booleans is

   type Boolean_Proxy is
     limited new VSS.XML.Templates.Proxies.Abstract_Value_Proxy with
   record
      Value : Boolean;
   end record;

   overriding function Value
     (Self : Boolean_Proxy) return VSS.XML.Templates.Values.Value;

end VSS.XML.Templates.Proxies.Booleans;
