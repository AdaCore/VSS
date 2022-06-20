--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Subprograms to simplify transition to VSS for software that use Ada's
--  standard string types.

with VSS.Strings;

package VSS.Utils.Conversions is

   function Decode
     (Item     : String;
      Encoding : String) return VSS.Strings.Virtual_String;
   --  Decode string with given encoding into Virtual_String. Raise
   --  Constraint_Error when encoding is not supported or some error occurred
   --  during data decoding.

end VSS.Utils.Conversions;
