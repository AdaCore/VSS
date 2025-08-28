--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Instantiation of generic integer formatter package for standard Integer
--  type.

with VSS.Strings.Formatters.Generic_Integers;

package VSS.Strings.Formatters.Integers is
  new VSS.Strings.Formatters.Generic_Integers (Integer);
pragma Preelaborate (VSS.Strings.Formatters.Integers);
