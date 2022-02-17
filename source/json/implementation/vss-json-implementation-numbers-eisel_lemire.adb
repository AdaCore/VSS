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

with Ada.Unchecked_Conversion;

with VSS.JSON.Implementation.Numbers.Tables;

package body VSS.JSON.Implementation.Numbers.Eisel_Lemire is

   use type Interfaces.Integer_32;
   use type Interfaces.Unsigned_32;

   function U32 is
     new Ada.Unchecked_Conversion
           (Interfaces.Integer_32, Interfaces.Unsigned_32);

   function I32 is
     new Ada.Unchecked_Conversion
           (Interfaces.Unsigned_32, Interfaces.Integer_32);

   function F64 is
     new Ada.Unchecked_Conversion
           (Interfaces.Unsigned_64, Interfaces.IEEE_Float_64);

   function clz
     (Value : Interfaces.Unsigned_64) return Interfaces.Integer_32
     with Import,
          Convention    => Intrinsic,
          External_Name => "__builtin_clzll";

   procedure Multiply
     (A : Interfaces.Unsigned_64;
      B : Interfaces.Unsigned_64;
      L : out Interfaces.Unsigned_64;
      H : out Interfaces.Unsigned_64);
   --  Multiplication of two 64-bit usnigned integers into 128-bit values,
   --  splitted into high and low 64-bit unsigned integers. On x86_64 it is
   --  optimized into single instruction.

   -------------
   -- Convert --
   -------------

   --  Produces the IEEE 754 double-precision value for an exact significand
   --  and base-10 exponent. For example:
   --   - when parsing "12345.678e+02", man is 12345678 and exp10 is -1.
   --   - when parsing "-12", man is 12 and exp10 is 0. Processing the leading
   --     minus sign is the responsibility of the caller, not this function.
   --
   --  The algorithm is based on an original idea by Michael Eisel that was
   --  refined by Daniel Lemire. See
   --  https://lemire.me/blog/2020/03/10/fast-float-parsing-in-practice/
   --
   --  Preconditions:
   --   - Significand is non-zero.
   --   - Exponent_10 is in the range [-307 ..= 288]
   --
   --  The Exponent_10 range (and the fact that significand is in the range
   --  [1 ..= UINT64_MAX], approximately [1 ..= 1.85e+19]) means that
   --  (Significand * (10 ** Exponent_10)) is in the range
   --  [1e-307 ..= 1.85e+307]. This is entirely within the range of normal
   --  (neither subnormal nor non-finite) f64 values: DBL_MIN and DBL_MAX are
   --  approximately 2.23e–308 and 1.80e+308.

   procedure Convert
     (Significand  : Interfaces.Unsigned_64;
      Exponent_10  : Interfaces.Integer_32;
      Number       : out Interfaces.IEEE_Float_64;
      Success      : out Boolean)
   is
      --  Names of variables corresponds to used in paper.

      W  : Interfaces.Unsigned_64          := Significand;
      Q  : constant Interfaces.Integer_32  := Exponent_10;
      L  : Interfaces.Integer_32;
      ZH : Interfaces.Unsigned_64;
      ZL : Interfaces.Unsigned_64;
      M  : Interfaces.Unsigned_64;
      P  : Interfaces.Integer_32;
      U  : Interfaces.Integer_32;

      YH : Interfaces.Unsigned_64;
      YL : Interfaces.Unsigned_64;
      MH : Interfaces.Unsigned_64;
      ML : Interfaces.Unsigned_64;

   begin
      if Q not in -307 .. 288 then
         Success := False;

         return;
      end if;

      --  Normalize the significand W. The (W != 0) precondition means that a
      --  non-zero bit exists.

      L := clz (W);
      W := Interfaces.Shift_Left (W, Natural (L));

      --  Calculate the return value's base-2 exponent. We might tweak it by
      --  ±1 later, but its initial value comes from a linear scaling of Q,
      --  converting from power-of-10 to power-of-2, and adjusting by L.
      --
      --  The magic constants are:
      --   - 1087 = 1023 + 64. The 1023 is the f64 exponent bias. The 64 is
      --     because the look-up table uses 64-bit mantissas.
      --   - 217706 is such that the ratio 217706 / 65536 ≈ 3.321930 is close
      --     enough (over the practical range of exp10) to log(10) / log(2) ≈
      --     3.321928.
      --   - 65536 = 1<<16 is arbitrary but a power of 2, so division is a
      --     shift.
      --
      --  Equality of the linearly-scaled value and the actual power-of-2,
      --  over the range of Q arguments that this function accepts, is
      --  confirmed by script/print-mpb-powers-of-10.go
      --
      --  Note, arithmetic shift is required here, it is available for
      --  modular types only, thus use unchecked conversion to convert
      --  signed integer into modular and back.

      P :=
        I32 (Interfaces.Shift_Right_Arithmetic (U32 (217706 * Q), 16))
          + 1_087 - L;

      --  Look up the (possibly truncated) base-2 representation of (10 ** Q).
      --  The look-up table was constructed so that it is already normalized:
      --  the table entry's significand's MSB (most significant bit) is on.
      --
      --  Multiply the two significands. Normalization means that both
      --  significands are at least (1<<63), so the 128-bit product must be at
      --  least (1<<126). The high 64 bits of the product, ZH, must therefore
      --  be at least (1<<62).
      --
      --  As a consequence, ZH has either 0 or 1 leading zeroes. Shifting ZH
      --  right by either 9 or 10 bits (depending on ZH's MSB) will therefore
      --  leave the top 10 MSBs (bits 54 ..= 63) off and the 11th MSB (bit 53)
      --  on.

      Multiply (W, Tables.Powers_Of_Ten (Q).L, ZL, ZH);

      --  Before we shift right by at least 9 bits, recall that the look-up
      --  table entry was possibly truncated. We have so far only calculated
      --  a lower bound for the product (W * e), where e is (10 ** Q). The
      --  upper bound would add a further (W * 1) to the 128-bit product,
      --  which overflows the lower 64-bit limb if ((ZL + W) < W).
      --
      --  If overflow occurs, that adds 1 to ZH. Since we're about to shift
      --  right by at least 9 bits, that carried 1 can be ignored unless the
      --  higher 64-bit limb's low 9 bits are all on.
      --
      --  For example, parsing "9999999999999999999" will take the if-true
      --  branch here, since:
      --   - ZH = 0x4563918244F3FFFF
      --   - ZL = 0x8000000000000000
      --   - W  = 0x8AC7230489E7FFFF

      if (ZH and 16#1FF#) = 16#1FF# and (ZL + W) < W then
      --  if (ZH and 16#1FF#) = 16#1FF# and (ZL + W) < ZL then
         --  Refine our calculation of (W * e). Before, our approximation of e
         --  used a "low resolution" 64-bit significand. Now use a "high
         --  resolution" 128-bit significand. We've already calculated
         --  Z = (W * bits_0_to_63_incl_of_e).
         --  Now calculate Y = (W * bits_64_to_127_incl_of_e).

         Multiply (W, Tables.Powers_Of_Ten (Q).H, YL, YH);

         --  Merge the 128-bit Z and 128-bit Y, which overlap by 64 bits, to
         --  calculate the 192-bit product of the 64-bit W by the 128-bit e.
         --  As we exit this if-block, we only care about the high 128 bits
         --  (MH and ML) of that 192-bit product.
         --
         --  For example, parsing "1.234e-45" will take the if-true branch
         --  here, since:
         --   - ZH = 0x70B7E3696DB29FFF
         --   - ZL = 0xE040000000000000
         --   - YH = 0x33718BBEAB0E0D7A
         --   - YL = 0xA880000000000000

         MH := ZH;
         ML := ZL + YH;

         if ML < ZL then
            MH := MH + 1;
         end if;

         --  The "high resolution" approximation of e is still a lower bound.
         --  Once again, see if the upper bound is large enough to produce a
         --  different result. This time, if it does, give up instead of
         --  reaching for an even more precise approximation to e.
         --
         --  This three-part check is similar to the two-part check that
         --  guarded the if block that we're now in, but it has an extra term
         --  for the middle 64 bits (checking that adding 1 to merged_lo would
         --  overflow).
         --
         --  For example, parsing "5.9604644775390625e-8" will take the
         --  if-true branch here, since:
         --   - MH = 0x7FFFFFFFFFFFFFFF
         --   - ML = 0xFFFFFFFFFFFFFFFF
         --   - YL = 0x4DB3FFC120988200
         --   - W  = 0xD3C21BCECCEDA100

         if (MH and 16#1FF#) = 16#1FF# and (ML + 1) = 0 and (YL + W) < W then
            Success := False;

            return;
         end if;

         --  Replace the 128-bit x with merged.

         ZH := MH;
         ZL := ML;
      end if;

      --  As mentioned above, shifting ZH right by either 9 or 10 bits will
      --  leave the top 10 MSBs (bits 54 ..= 63) off and the 11th MSB (bit 53)
      --  on. If the MSB (before shifting) was on, adjust P for the larger
      --  shift.
      --
      --  Having bit 53 on (and higher bits off) means that M is a 54-bit
      --  number.

      U := Interfaces.Integer_32 (Interfaces.Shift_Right (ZH, 63));
      M := Interfaces.Shift_Right (ZH, Natural (U + 9));
      P := P - I32 (1 xor U32 (U));

      --  IEEE 754 rounds to-nearest with ties rounded to-even. Rounding
      --  to-even can be tricky. If we're half-way between two exactly
      --  representable numbers (x's low 73 bits are zero and the next 2 bits
      --  that matter are "01"), give up instead of trying to pick the winner.
      --
      --  Technically, we could tighten the condition by changing "73" to "73
      --  or 74, depending on msb", but a flat "73" is simpler.
      --
      --  For example, parsing "1e+23" will take the if-true branch here,
      --  since:
      --   - ZH = 0x54B40B1F852BDA00
      --   - M  = 0x002A5A058FC295ED

      if ZL = 0 and (ZH and 16#1FF#) = 0 and (M and 2#11#) = 2#01# then
         Success := False;

         return;
      end if;

      --  If we're not halfway then it's rounding to-nearest. Starting with a
      --  54-bit number, carry the lowest bit (bit 0) up if it's on.
      --  Regardless of whether it was on or off, shifting right by one then
      --  produces a 53-bit number. If carrying up overflowed, shift again.

      M := M + (M and 1);
      M := Interfaces.Shift_Right (M, 1);

      --  This if block is equivalent to (but benchmarks slightly faster
      --  than) the following branchless form:
      --     uint64_t overflow_adjustment = ret_mantissa >> 53;
      --     ret_mantissa >>= overflow_adjustment;
      --     ret_exp2 += overflow_adjustment;
      --
      --  For example, parsing "7.2057594037927933e+16" will take the if-true
      --  branch here, since:
      --   - ZH = 0x7FFFFFFFFFFFFE80
      --   - M  = 0x0020000000000000

      if Interfaces.Shift_Right (M, 53) > 0 then
         M := Interfaces.Shift_Right (M, 1);
         P := P + 1;
      end if;

      --  Starting with a 53-bit number, IEEE 754 double-precision normal
      --  numbers have an implicit mantissa bit. Mask that away and keep the
      --  low 52 bits.

      M := M and 16#000F_FFFF_FFFF_FFFF#;

      --  Pack the bits and return.

      Number  :=
        F64 (M or (Interfaces.Shift_Left (Interfaces.Unsigned_64 (P), 52)));
      Success := True;
   end Convert;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (A : Interfaces.Unsigned_64;
      B : Interfaces.Unsigned_64;
      L : out Interfaces.Unsigned_64;
      H : out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_128;

      R : constant Interfaces.Unsigned_128 :=
        Interfaces.Unsigned_128 (A) * Interfaces.Unsigned_128 (B);

   begin
      L := Interfaces.Unsigned_64 (R mod 2 ** 64);
      H := Interfaces.Unsigned_64 (R / 2 ** 64);
   end Multiply;

end VSS.JSON.Implementation.Numbers.Eisel_Lemire;
