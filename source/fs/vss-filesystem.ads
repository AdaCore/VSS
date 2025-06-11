--
--  Copyright (C) 2025
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  API for elementary filesystem interaction.

with VSS.Strings;

package VSS.Filesystem is

   function Exists (Path : Ada.Strings.Virtual_String) return Boolean;

end package;
