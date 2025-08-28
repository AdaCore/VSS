--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Loads PropertyValueAliases.txt file. Two stages are used to load
--  information, at the first stage only aliases are loaded and property-values
--  data structures are constructed, and @missing directives are ignored. This
--  information is necessary to construct internal character database. At the
--  second stage internal character database are initialized and only @missing
--  directives are processed to fill it.

package UCD.Property_Value_Aliases_Loader is

   procedure Load_Aliases (UCD_Root : Wide_Wide_String);
   --  Load aliases and ignore @missing directives.

   procedure Load_Missing (UCD_Root : Wide_Wide_String);
   --  Load @missing directives only.

end UCD.Property_Value_Aliases_Loader;
