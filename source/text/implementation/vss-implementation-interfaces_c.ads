--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Utilities to interface with C language.

with VSS.Unicode;

package VSS.Implementation.Interfaces_C with Preelaborate is

   type UTF8_Code_Unit_Constant_Access is
     access constant VSS.Unicode.UTF8_Code_Unit with Convention => C;

end VSS.Implementation.Interfaces_C;
