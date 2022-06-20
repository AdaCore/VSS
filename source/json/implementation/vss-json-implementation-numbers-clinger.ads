--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Fast conversion when value of decimal significand and decimal exponent
--  are small enough to be exactly represented by floating point numbers and
--  their multiplication provides exact result.

package VSS.JSON.Implementation.Numbers.Clinger is

   pragma Preelaborate;

   procedure Convert
     (Significand  : Interfaces.Unsigned_64;
      Exponent_10  : Interfaces.Integer_32;
      Number       : out Interfaces.IEEE_Float_64;
      Success      : out Boolean);

end VSS.JSON.Implementation.Numbers.Clinger;
