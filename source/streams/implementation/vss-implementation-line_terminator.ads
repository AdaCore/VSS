--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Constants for line termination sequences.

with VSS.Strings;

package VSS.Implementation.Line_Terminator is

   pragma Preelaborate;

   function Sequence
     (Terminator : VSS.Strings.Line_Terminator)
      return VSS.Strings.Virtual_String;

end VSS.Implementation.Line_Terminator;
