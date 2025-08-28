--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Some utility subprograms.

package VSS.Strings.Utilities is

   pragma Preelaborate;

   function Display_Width
     (Item : VSS.Strings.Virtual_String)
      return VSS.Strings.Display_Cell_Count;
   --  Computes display cell width of the given string.

end VSS.Strings.Utilities;
