--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package VSS.Regular_Expressions.Utilities is

   pragma Preelaborate;

   function Escape
     (Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Escapes all characters of the Item that they don't have any special
   --  meaning when used as regular expression pattern string and returns
   --  escaped string.

   function Wildcard_To_Regular_Expression
     (Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Returns a regular expression representation of the glob pattern Item.

end VSS.Regular_Expressions.Utilities;
