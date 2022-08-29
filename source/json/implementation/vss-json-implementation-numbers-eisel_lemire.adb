--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Unchecked_Conversion;

with VSS.JSON.Implementation.Numbers.Tables;

package body VSS.JSON.Implementation.Numbers.Eisel_Lemire is

   use type Interfaces.Integer_32;
   use type Interfaces.Unsigned_32;

   subtype Extended_Float is Decoded_Float;

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
   --  Multiplication of two 64-bit unsigned integers into 128-bit values,
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

   function Exponent_To_Power
     (Q : Interfaces.Integer_32) return Interfaces.Integer_32;
   --  Convert decimal exponent to binary power.

   procedure Compute_Error_Scaled
     (Mantissa      : Interfaces.Unsigned_64;
      Exponent      : Interfaces.Integer_32;
      Leading_Zeros : Interfaces.Integer_32;
      Number        : out Decoded_Float);
   --  Create an adjusted mantissa, biased by the invalid power2
   --  for significant digits already multiplied by 10 ** q.

   procedure Halfway
     (Value  : Interfaces.IEEE_Float_64;
      Number : out Extended_Float);
   --  Get the extended precision value of the halfway point between b and
   --  b+u. We are given a native float that represents b, so we need to
   --  adjust it halfway between b and b+u.

   procedure Decode_IEEE_Float
     (Encoded : Interfaces.IEEE_Float_64;
      Decoded : out Extended_Float);
   --  Converts a native floating-point number to an extended-precision float.

   generic
      with procedure Callback
        (Number : in out Decoded_Float; Shift : Interfaces.Integer_32);

   procedure Generic_Round (Number : in out Decoded_Float);
   --  Round an extended-precision float to the nearest machine float.

   generic
      with function Callback
        (Is_Odd     : Boolean;
         Is_Halfway : Boolean;
         Is_Above   : Boolean) return Boolean;

   procedure Generic_Round_Nearest_Tie_Even
     (Number : in out Extended_Float;
      Shift  : Interfaces.Integer_32)
     with Pre => Shift in 1 .. Interfaces.Unsigned_64'Size;

   -------------------
   -- Compute_Error --
   -------------------

   procedure Compute_Error
     (Mantissa : Interfaces.Unsigned_64;
      Exponent : Interfaces.Integer_32;
      Number   : out Decoded_Float)
   is
      W  : Interfaces.Unsigned_64         := Mantissa;
      Q  : constant Interfaces.Integer_32 := Exponent;
      L  : constant Interfaces.Integer_32 := clz (W);
      ZH : Interfaces.Unsigned_64;
      ZL : Interfaces.Unsigned_64;

   begin
      W := Interfaces.Shift_Left (W, Natural (L));

      Compute_Product_Approximation (W, Q, ZL, ZH);
      Compute_Error_Scaled (ZH, Q, L, Number);
   end Compute_Error;

   --------------------------
   -- Compute_Error_Scaled --
   --------------------------

   procedure Compute_Error_Scaled
     (Mantissa           : Interfaces.Unsigned_64;
      Exponent           : Interfaces.Integer_32;
      Leading_Zeros      : Interfaces.Integer_32;
      Number             : out Decoded_Float)
   is
      Bias               : constant :=
        Mantissa_Explicit_Bits - Minimum_Exponent;
      High_Leading_Zeros : constant Interfaces.Integer_32 :=
        Interfaces.Integer_32 ((Interfaces.Shift_Right (Mantissa, 63) xor 1));

   begin
      Number.Significand :=
        Interfaces.Shift_Left (Mantissa, Natural (High_Leading_Zeros));

      Number.Power :=
        Exponent_To_Power (Exponent)
          + Bias - High_Leading_Zeros - Leading_Zeros - 62 + Invalid_Bias;
   end Compute_Error_Scaled;

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
      Number       : out Decoded_Float;
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
            Compute_Error_Scaled (ZH, Q, L, Number);

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
      P := Exponent_To_Power (Q) + U - L - Minimum_Exponent;

      if P <= 0 then
         --  we have a subnormal?

         --  Here have that answer.power2 <= 0 so -answer.power2 >= 0

         if -P + 1 >= 64 then
            --  if we have more than 64 bits below the minimum exponent, you
            --  have a zero for sure.

            P := 0;
            M := 0;

            Number := (M, P);
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

         Number := (M, P);
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

      Number := (M, P);
      Success := True;
   end Convert;

   -----------------------
   -- Decode_IEEE_Float --
   -----------------------

   procedure Decode_IEEE_Float
     (Encoded : Interfaces.IEEE_Float_64;
      Decoded : out Extended_Float)
   is
      Bias : constant := Mantissa_Explicit_Bits - Minimum_Exponent;

      Encoded_Unsigned : Interfaces.Unsigned_64
        with Address => Encoded'Address;

   begin
      if (Encoded_Unsigned and Exponent_Mask) = 0 then
         --  Denormal

         Decoded :=
           (Encoded_Unsigned and Mantissa_Mask,
            1);

      else
         --  Normal

         Decoded :=
           ((Encoded_Unsigned and Mantissa_Mask) or Hidden_Bit_Mask,
            Interfaces.Integer_32
              (Interfaces.Shift_Right
                   (Encoded_Unsigned and Exponent_Mask,
                    Mantissa_Explicit_Bits)));
      end if;

      Decoded.Power := @ - Bias;
   end Decode_IEEE_Float;

   -----------------------
   -- Exponent_To_Power --
   -----------------------

   function Exponent_To_Power
     (Q : Interfaces.Integer_32) return Interfaces.Integer_32 is
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
   end Exponent_To_Power;

   -------------------
   -- Generic_Round --
   -------------------

   procedure Generic_Round (Number : in out Decoded_Float) is
      Mantissa_Shift : constant := 64 - Mantissa_Explicit_Bits - 1;
      Shift          : Interfaces.Integer_32;

   begin
      if -Number.Power >= Mantissa_Shift then
         --  Have a denormal float

         Shift := -Number.Power + 1;

         Callback (Number, Interfaces.Integer_32'Min (Shift, 64));

         Number.Power :=
           (if Number.Significand
                 < Interfaces.Shift_Left (1, Mantissa_Explicit_Bits)
            then 0
            else 1);
         --  Check for round-up: if rounding-nearest carried us to the hidden
         --  bit.

         return;
      end if;

      --  Have a normal float, use the default shift.

      Callback (Number, Mantissa_Shift);

      --  Check for carry

      if Number.Significand
           >= Interfaces.Shift_Left (2, Mantissa_Explicit_Bits)
      then
         Number.Significand :=
           Interfaces.Shift_Left (1, Mantissa_Explicit_Bits);
         Number.Power := @ + 1;
      end if;

      --  Check for infinite: we could have carried to an infinite power.

      Number.Significand :=
        @ and not Interfaces.Shift_Left (1, Mantissa_Explicit_Bits);

      if Number.Power >= Infinite_Power then
         Number.Power := Infinite_Power;
         Number.Significand := 0;

         raise Program_Error;
      end if;
   end Generic_Round;

   ------------------------------------
   -- Generic_Round_Nearest_Tie_Even --
   ------------------------------------

   procedure Generic_Round_Nearest_Tie_Even
     (Number : in out Decoded_Float;
      Shift  : Interfaces.Integer_32)
   is
      Mask           : constant Interfaces.Unsigned_64 :=
        (if Shift = Interfaces.Unsigned_64'Size
           then Interfaces.Unsigned_64'Last
           else Interfaces.Shift_Left (1, Natural (Shift)) - 1);
      Halfway        : constant Interfaces.Unsigned_64 :=
        (if Shift = 0
           then 0
           else Interfaces.Shift_Left (1, Natural (Shift - 1)));
      Truncated_Bits : constant Interfaces.Unsigned_64 :=
        Number.Significand and Mask;
      Is_Above       : constant Boolean := Truncated_Bits > Halfway;
      Is_Halfway     : constant Boolean := Truncated_Bits = Halfway;
      Is_Odd         : Boolean;

   begin
      --  Shift digits into position

      if Shift = Interfaces.Unsigned_64'Size then
         Number.Significand := 0;

      else
         Number.Significand :=
           Interfaces.Shift_Right (Number.Significand, Natural (Shift));
      end if;

      Number.Power := @ + Shift;
      Is_Odd := (Number.Significand and 2#1#) = 2#1#;

      Number.Significand :=
        @ + (if Callback (Is_Odd, Is_Halfway, Is_Above) then 1 else 0);
   end Generic_Round_Nearest_Tie_Even;

   -------------
   -- Halfway --
   -------------

   procedure Halfway
     (Value  : Interfaces.IEEE_Float_64;
      Number : out Extended_Float) is
   begin
      Decode_IEEE_Float (Value, Number);

      Number.Significand := Interfaces.Shift_Left (Number.Significand, 1);
      Number.Significand := @ + 1;
      Number.Power       := @ - 1;
   end Halfway;

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

   --------------------
   -- Scale_Negative --
   --------------------

   procedure Scale_Negative
     (Mantissa : VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Exponent : Interfaces.Integer_32;
      Error    : VSS.JSON.Implementation.Numbers.Decoded_Float;
      Number   : out Decoded_Float)
   is
      Real_Digits  : VSS.JSON.Implementation.Big_Integers.Big_Integer :=
        Mantissa;
      Real_Exp     : constant Interfaces.Integer_32 := Exponent;
      Aux          : Decoded_Float := Error;
      B            : Interfaces.IEEE_Float_64;
      Theor        : Extended_Float;
      Theor_Digits : VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Theor_Exp    : Interfaces.Integer_32;
      Pow2_Exp     : Interfaces.Integer_32;
      Pow5_Exp     : Interfaces.Integer_32;
      Order        : VSS.JSON.Implementation.Big_Integers.Compare_Kind;

      function Adjust
        (Is_Odd     : Boolean;
         Is_Halfway : Boolean;
         Is_Above   : Boolean) return Boolean;

      procedure Round_Down
        (Number : in out Decoded_Float; Shift : Interfaces.Integer_32);

      ------------
      -- Adjust --
      ------------

      function Adjust
        (Is_Odd     : Boolean;
         Is_Halfway : Boolean;
         Is_Above   : Boolean) return Boolean
      is
         pragma Unreferenced (Is_Halfway);
         pragma Unreferenced (Is_Above);

         use all type VSS.JSON.Implementation.Big_Integers.Compare_Kind;

      begin
         if Order = Greater then
            return True;

         elsif Order = Less then
            return False;

         else
            return Is_Odd;
         end if;
      end Adjust;

      ----------------
      -- Round_Down --
      ----------------

      procedure Round_Down
        (Number : in out Decoded_Float; Shift : Interfaces.Integer_32) is
      begin
         if Shift = 64 then
            raise Program_Error;

         else
            Number.Significand :=
              Interfaces.Shift_Right (Number.Significand, Natural (Shift));
         end if;

         Number.Power := @ + Shift;
      end Round_Down;

      procedure Round_Nearest_Tie_Even is
        new Generic_Round_Nearest_Tie_Even (Adjust);

      procedure Round_Nearest_Tie_Even is
        new Generic_Round (Round_Nearest_Tie_Even);

      procedure Round_Down is new Generic_Round (Round_Down);

   begin
      --  Get the value of `b`, rounded down, and get a bigint representation
      --  of b+h

      Round_Down (Aux);
      Encode_IEEE_Float (Aux, False, B);

      Halfway (B, Theor);
      VSS.JSON.Implementation.Big_Integers.Set
        (Theor_Digits, Theor.Significand);

      Theor_Exp := Theor.Power;

      --  Scale real digits and theor digits to be same power.

      Pow2_Exp := Theor_Exp - Real_Exp;
      Pow5_Exp := -Real_Exp;

      if Pow5_Exp /= 0 then
         VSS.JSON.Implementation.Big_Integers.Multiply_Power_5
           (Theor_Digits, Pow5_Exp);
      end if;

      if Pow2_Exp > 0 then
         VSS.JSON.Implementation.Big_Integers.Multiply_Power_2
           (Theor_Digits, Pow2_Exp);

      elsif Pow2_Exp < 0 then
         VSS.JSON.Implementation.Big_Integers.Multiply_Power_2
           (Real_Digits, -Pow2_Exp);
      end if;

      --  Compare digits, and use it to director rounding

      Order :=
        VSS.JSON.Implementation.Big_Integers.Compare
          (Real_Digits, Theor_Digits);
      Number := Error;

      Round_Nearest_Tie_Even (Number);
   end Scale_Negative;

   --------------------
   -- Scale_Positive --
   --------------------

   procedure Scale_Positive
     (Mantissa : in out VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Exponent : Interfaces.Integer_32;
      Number   : out Decoded_Float)
   is
      Bias      : constant := Mantissa_Explicit_Bits - Minimum_Exponent;
      Truncated : Boolean;

      function Adjust
        (Is_Odd     : Boolean;
         Is_Halfway : Boolean;
         Is_Above   : Boolean) return Boolean;

      ------------
      -- Adjust --
      ------------

      function Adjust
        (Is_Odd     : Boolean;
         Is_Halfway : Boolean;
         Is_Above   : Boolean) return Boolean is
      begin
         return
           Is_Above
             or (Is_Halfway and Truncated)
             or (Is_Odd and Is_Halfway);
      end Adjust;

      procedure Round_Nearest_Tie_Even is
        new Generic_Round_Nearest_Tie_Even (Adjust);

      procedure Round is new Generic_Round (Round_Nearest_Tie_Even);

   begin
      VSS.JSON.Implementation.Big_Integers.Multiply_Power_10
        (Mantissa, Exponent);
      VSS.JSON.Implementation.Big_Integers.Get_High_64
        (Mantissa, Number.Significand, Truncated);

      Number.Power :=
        VSS.JSON.Implementation.Big_Integers.Size (Mantissa) - 64 + Bias;

      Round (Number);
   end Scale_Positive;

end VSS.JSON.Implementation.Numbers.Eisel_Lemire;
