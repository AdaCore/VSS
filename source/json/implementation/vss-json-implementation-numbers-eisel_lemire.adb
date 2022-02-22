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

pragma Warnings (Off, "unrecognized pragma");
pragma Ada_2020;
pragma Ada_2022;
pragma Warnings (On, "unrecognized pragma");

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

   procedure Compute_Product_Approximation
     (W       : Interfaces.Unsigned_64;
      Q       : Interfaces.Integer_32;
      L       : out Interfaces.Unsigned_64;
      H       : out Interfaces.Unsigned_64);
   --  Compute or rather approximate w * 5**q and return a pair of
   --  64-bit words approximating the result, with the "high" part
   --  corresponding to the most significant bits and the low part
   --  corresponding to the least significant bits.

   function Power (Q : Interfaces.Integer_32) return Interfaces.Integer_32;

   procedure To_Binary_Float
     (M : Interfaces.Unsigned_64;
      P : Interfaces.Integer_32;
      N : out Interfaces.IEEE_Float_64);

   -----------------------------------
   -- Compute_Product_Approximation --
   -----------------------------------

   procedure Compute_Product_Approximation
     (W       : Interfaces.Unsigned_64;
      Q       : Interfaces.Integer_32;
      L       : out Interfaces.Unsigned_64;
      H       : out Interfaces.Unsigned_64)
   is
      Bit_Precision  : constant := Mantissa_Explicit_Bits + 3;

      Precision_Mask : constant Interfaces.Unsigned_64 :=
        (if Bit_Precision < 64
         then Interfaces.Shift_Right (16#FFFF_FFFF_FFFF_FFFF#, Bit_Precision)
         else 16#FFFF_FFFF_FFFF_FFFF#);

      FL : Interfaces.Unsigned_64;
      FH : Interfaces.Unsigned_64;
      SL : Interfaces.Unsigned_64;
      SH : Interfaces.Unsigned_64;

   begin
      --  For small values of q, e.g., q in [0,27], the answer is always exact
      --  because the first call of Multiply gives the exact answer.

      Multiply (W, Tables.Powers_Of_Five (Q).L, FL, FH);

      if (FH and Precision_Mask) = Precision_Mask then
         --  could further guard with  (lower + w < lower)

         --  regarding the second product, we only need SH, but our
         --  expectation is that the compiler will optimize this extra
         --  work away if needed.

         Multiply (W, Tables.Powers_Of_Five (Q).H, SL, SH);
         FL := @ + SH;

         if SH > FL then
            FH := @ + 1;
         end if;
      end if;

      L := FL;
      H := FH;
   end Compute_Product_Approximation;

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
      --  Names of objects correspond to used in paper.

      W  : Interfaces.Unsigned_64          := Significand;
      Q  : constant Interfaces.Integer_32  := Exponent_10;
      L  : Interfaces.Integer_32;
      U  : Interfaces.Integer_32;
      ZH : Interfaces.Unsigned_64;
      ZL : Interfaces.Unsigned_64;
      M  : Interfaces.Unsigned_64;
      P  : Interfaces.Integer_32;

   begin
      --  Corner cases of zero and infinity are processed before call of this
      --  subprogram and thus omitted here.

      --  We want the most significant bit of W to be 1. Shift if needed.
      L := clz (W);
      W := Interfaces.Shift_Left (W, Natural (L));

      --  The required precision is Mantissa_Explicit_Bits + 3 because
      --  1. We need the implicit bit
      --  2. We need an extra bit for rounding purposes
      --  3. We might lose a bit due to the "upperbit" routine (result too
      --  small, requiring a shift)

      Compute_Product_Approximation (W, Q, ZL, ZH);

      if ZL = 16#FFFF_FFFF_FFFF_FFFF# then
         --  could guard it further

         --  In some very rare cases, this could happen, in which case we
         --  might need a more accurate computation that what we can provide
         --  cheaply. This is very, very unlikely.

         if Q not in -27 .. 55 then
            Success := False;

            return;
         end if;

         --  Always good because 5**q <2**128 when q>=0, and otherwise, for
         --  q<0, we have 5**-q<2**64 and the 128-bit reciprocal allows for
         --  exact computation.
      end if;

      --  The Compute_Product_Approximation subprogram can be slightly slower
      --  than a branchless approach:
      --
      --    value128 product = compute_product(q, w);
      --
      --  but in practice, we can win big with the
      --  Compute_Product_Approximation if its additional branch is easily
      --  predicted. Which is best is data specific.

      U := Interfaces.Integer_32 (Interfaces.Shift_Right (ZH, 63));
      M :=
        Interfaces.Shift_Right
          (ZH, Natural (U + 64 - Mantissa_Explicit_Bits - 3));
      P := Power (Q) + U - L - Minimum_Exponent;

      if P <= 0 then
         --  we have a subnormal?

         --  Here have that answer.power2 <= 0 so -answer.power2 >= 0

         if -P + 1 >= 64 then
            --  if we have more than 64 bits below the minimum exponent, you
            --  have a zero for sure.

            P := 0;
            M := 0;

            To_Binary_Float (M, P, Number);
            Success := True;

            return;
         end if;

         --  Next line is safe because -answer.power2 + 1 < 64

         M := Interfaces.Shift_Right (M, Natural (-P + 1));

         --  Thankfully, we can't have both "round-to-even" and subnormals
         --  because "round-to-even" only occurs for powers close to 0.

         M := @ + (M and 1);   --  Round up
         M := Interfaces.Shift_Right (M, 1);

         --  There is a weird scenario where we don't have a subnormal but
         --  just. Suppose we start with 2.2250738585072013e-308, we end up
         --  with 0x3fffffffffffff x 2^-1023-53 which is technically subnormal
         --  whereas 0x40000000000000 x 2^-1023-53 is normal. Now, we need to
         --  round up 0x3fffffffffffff x 2^-1023-53 and once we do, we are no
         --  longer subnormal, but we can only know this after rounding.
         --  So we only declare a subnormal if we are smaller than the
         --  threshold.

         P :=
           (if M < Interfaces.Shift_Left (1, Mantissa_Explicit_Bits)
            then 0
            else 1);

         To_Binary_Float (M, P, Number);
         Success := True;

         return;
      end if;

      --  Usually, we round *up*, but if we fall right in between and and we
      --  have an even basis, we need to round down.
      --
      --  We are only concerned with the cases where 5**q fits in single
      --  64-bit word.
      if ZL <= 1
        and Q in Min_Exponent_Round_To_Even .. Max_Exponent_Round_To_Even
        and (M and 2#11#) = 2#01#
      then
         --  We may fall between two floats!

         --  To be in-between two floats we need that in doing
         --
         --     answer.mantissa =
         --       product.high >>
         --         (upperbit + 64 - binary::mantissa_explicit_bits() - 3);
         --
         --  ... we dropped out only zeroes. But if this happened, then we can
         --  go back!!!

         if Interfaces.Shift_Left
              (M, Natural (U + 64 - Mantissa_Explicit_Bits - 3))
           = ZH
         then
            M := @ and not 1;  --  Flip it so that we do not round up
         end if;
      end if;

      M := @ + (M and 1);  --  round up
      M := Interfaces.Shift_Right (M, 1);

      if M >= Interfaces.Shift_Left (2, Mantissa_Explicit_Bits) then
         M := Interfaces.Shift_Left (1, Mantissa_Explicit_Bits);
         P := @ + 1;  --  Undo previous addition
      end if;

      M := @ and not Interfaces.Shift_Left (1, Mantissa_Explicit_Bits);

      if P >= Infinite_Power then
         --  infinity

         P := Infinite_Power;
         M := 0;
      end if;

      To_Binary_Float (M, P, Number);
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

   -----------
   -- Power --
   -----------

   function Power (Q : Interfaces.Integer_32) return Interfaces.Integer_32 is
   begin
      --  For q in (0,350), we have that
      --
      --     f = (((152170 + 65536) * q ) >> 16);
      --
      --  is equal to
      --
      --     floor(p) + q
      --
      --  where
      --
      --     p = log(5**q)/log(2) = q * log(5)/log(2)
      --
      --  For negative values of q in (-400,0), we have that
      --
      --     f = (((152170 + 65536) * q ) >> 16);
      --
      --  is equal to
      --
      --     -ceil(p) + q
      --
      --  where
      --
      --     p = log(5**-q)/log(2) = -q * log(5)/log(2)

      --  Note, arithmetic shift is required here, it is available for
      --  modular types only, thus use unchecked conversion to convert
      --  signed integer into modular and back.

      return
        I32
          (Interfaces.Shift_Right_Arithmetic (U32 ((152170 + 65536) * Q), 16))
        + 63;
   end Power;

   ---------------------
   -- To_Binary_Float --
   ---------------------

   procedure To_Binary_Float
     (M : Interfaces.Unsigned_64;
      P : Interfaces.Integer_32;
      N : out Interfaces.IEEE_Float_64)
   is
      N_U64 : Interfaces.Unsigned_64 with Address => N'Address;
      --  This subprogram should be able to process Inf values, which is not
      --  valid value of floating point type in Ada, thus exception is raised
      --  in validity checks mode. To prevent this overlapped variable is used.

   begin
      N_U64 :=
        M
          or Interfaces.Shift_Left
               (Interfaces.Unsigned_64 (P), Mantissa_Explicit_Bits);
   end To_Binary_Float;

end VSS.JSON.Implementation.Numbers.Eisel_Lemire;
