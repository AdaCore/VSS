--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Instantiation of generic integer formatter package for standard Boolean
--  type.

with VSS.Strings.Formatters.Generic_Enumerations;

package VSS.Strings.Formatters.Booleans is
  new VSS.Strings.Formatters.Generic_Enumerations (Boolean);
pragma Preelaborate (VSS.Strings.Formatters.Booleans);
