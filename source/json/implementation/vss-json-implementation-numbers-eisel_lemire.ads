------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
--  Conversion of a decimal 64-bit unsigned integer exact significand and
--  32-bit signed decimal exponent into 64-bit IEEE 754 floating point format.
--
--  Algorithm is described in paper https://arxiv.org/pdf/2101.11408.pdf,
--  blog post https://nigeltao.github.io/blog/2020/eisel-lemire.html, and
--  reference implementation https://github.com/fastfloat/fast_float

with VSS.JSON.Implementation.Big_Integers;

private package VSS.JSON.Implementation.Numbers.Eisel_Lemire is

   pragma Preelaborate;

   use type Interfaces.Unsigned_64;

   Smallest_Power_Of_Ten      : constant := -342;     --  -65
   Largest_Power_Of_Ten       : constant := 308;      --  38
   Infinite_Power             : constant := 16#7FF#;  --  16#FF#
   Minimum_Exponent           : constant := -1023;    --  -127
   Mantissa_Explicit_Bits     : constant := 52;       --  23
   Min_Exponent_Round_To_Even : constant := -4;       --  -17
   Max_Exponent_Round_To_Even : constant := 23;       --  10;

   Exponent_Mask   : constant := 16#7FF0_0000_0000_0000#;  --  16#7F80_0000#
   Mantissa_Mask   : constant := 16#000F_FFFF_FFFF_FFFF#;  --  16#007F_FFFF#
   Hidden_Bit_Mask : constant := 16#0010_0000_0000_0000#;  --  16#0080_0000#

   Invalid_Bias               : constant := -16#8000#;

   procedure Convert
     (Significand  : Interfaces.Unsigned_64;
      Exponent_10  : Interfaces.Integer_32;
      Number       : out Decoded_Float;
      Success      : out Boolean)
     with Pre =>
       Significand /= 0
         and Exponent_10 in Smallest_Power_Of_Ten .. Largest_Power_Of_Ten;
   --  Attempt to do conversion. On success, set Number parameter to result
   --  value is computed, and set Success parameter to True. Set Success
   --  parameter to False if algorithm fails for any reasons.

   procedure Compute_Error
     (Mantissa : Interfaces.Unsigned_64;
      Exponent : Interfaces.Integer_32;
      Number   : out Decoded_Float);
   --  w * 10 ** q, without rounding the representation up.
   --  the power2 in the exponent will be adjusted by invalid_am_bias.

   procedure Scale_Positive
     (Mantissa : in out VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Exponent : Interfaces.Integer_32;
      Number   : out Decoded_Float);

   procedure Scale_Negative
     (Mantissa : VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Exponent : Interfaces.Integer_32;
      Error    : VSS.JSON.Implementation.Numbers.Decoded_Float;
      Number   : out Decoded_Float);
   --  The scaling here is quite simple: we have, for the real digits
   --  `m * 10^e`, and for the theoretical digits `n * 2^f`. Since `e`
   --  is always negative, to scale them identically, we do `n * 2^f * 5^-f`,
   --  so we now have `m * 2^e`. We then need to scale by `2^(f- e)`, and
   --  then the two significant digits are of the same magnitude.

end VSS.JSON.Implementation.Numbers.Eisel_Lemire;
