--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

package body VSS.JSON.Implementation.Packed_Decimals is

   use type Interfaces.Integer_32;
   use type Interfaces.Unsigned_64;

   Number_Of_U8_Limbs : constant :=
     Number_Of_U64_Limbs * Interfaces.Unsigned_64'Size
       / Interfaces.Unsigned_8'Size;
   --  Amount of 8-bit limbs to store packed decimal number of required
   --  size.

   type U8_Array is
     array (Interfaces.Unsigned_32 range 0 .. Number_Of_U8_Limbs - 1)
       of Interfaces.Unsigned_8;

   Digits_In_U64 : constant :=
     Interfaces.Unsigned_64'Size / Decimal_Digit'Value_Size;
   Digits_In_U8  : constant :=
     Interfaces.Unsigned_8'Size / Decimal_Digit'Value_Size;

   Power_Of_10 : constant
     array (Interfaces.Unsigned_32 range 1 .. 16) of Interfaces.Unsigned_64 :=
     [10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      100000000,
      1000000000,
      10000000000,
      100000000000,
      1000000000000,
      10000000000000,
      100000000000000,
      1000000000000000,
      10000000000000000];

   procedure Append
     (U64    : in out Unsigned_64_Array;
      Offset : U64_Limb_Offset;
      Count  : in out Interfaces.Unsigned_32;
      Digit  : Decimal_Digit);

   procedure Decode
     (Limb  : Interfaces.Unsigned_64;
      Count : Interfaces.Unsigned_32;
      Value : out Interfaces.Unsigned_64);

   procedure Add
     (Mantissa : in out VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Power    : Interfaces.Unsigned_64;
      Value    : Interfaces.Unsigned_64);

   ---------
   -- Add --
   ---------

   procedure Add
     (Mantissa : in out VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Power    : Interfaces.Unsigned_64;
      Value    : Interfaces.Unsigned_64) is
   begin
      VSS.JSON.Implementation.Big_Integers.Multiply (Mantissa, Power);
      VSS.JSON.Implementation.Big_Integers.Add (Mantissa, Value);
   end Add;

   ------------
   -- Append --
   ------------

   procedure Append
     (U64    : in out Unsigned_64_Array;
      Offset : U64_Limb_Offset;
      Count  : in out Interfaces.Unsigned_32;
      Digit  : Decimal_Digit)
   is
      use type Interfaces.Unsigned_8;

      U8        : U8_Array with Address => U64'Address;
      U8_Index  : constant Interfaces.Unsigned_32 :=
        (Count + Interfaces.Unsigned_32 (Offset * Digits_In_U64))
           / Digits_In_U8;

   begin
      if Count mod Digits_In_U8 = 0 then
         U8 (U8_Index) := Interfaces.Unsigned_8 (Digit);

      else
         U8 (U8_Index) := @ or Interfaces.Unsigned_8 (Digit) * 16;
      end if;

      Count := Count + 1;
   end Append;

   --------------------------
   -- Append_Decimal_Point --
   --------------------------

   procedure Append_Decimal_Point (Self  : in out Packed_Decimal) is
   begin
      pragma Assert (Self.Fractional_Offset = U64_Limb_Offset'Last);

      Self.Fractional_Offset :=
        U64_Limb_Offset
          ((Self.Integral_Count * Decimal_Digit'Value_Size
              + Interfaces.Unsigned_64'Size - 1)
           / Interfaces.Unsigned_64'Size);
   end Append_Decimal_Point;

   -----------------------
   -- Append_Fractional --
   -----------------------

   procedure Append_Fractional
     (Self  : in out Packed_Decimal;
      Digit : Decimal_Digit) is
   begin
      if Digit = 0
        and Self.Integral_Count = 0
        and Self.Fractional_Count = 0
      then
         --  Ignore all leading zeros, but adjust exponent.

         Self.Exponent_Adjustment := @ - 1;

      elsif Self.Integral_Count + Self.Fractional_Count = Number_Of_Digits then
         Self.Truncated := True;

      else
         Append
           (Self.U64, Self.Fractional_Offset, Self.Fractional_Count, Digit);

         if Digit /= 0 then
            Self.Last_Non_Zero := Self.Fractional_Count;
         end if;
      end if;
   end Append_Fractional;

   ---------------------
   -- Append_Integral --
   ---------------------

   procedure Append_Integral
     (Self  : in out Packed_Decimal;
      Digit : Decimal_Digit) is
   begin
      --  Ignore all leading zeros.

      if Digit /= 0 or Self.Integral_Count /= 0 then
         if Self.Integral_Count >= Number_Of_Digits then
            Self.Integral_Count      := @ + 1;
            Self.Exponent_Adjustment := @ + 1;
            Self.Truncated           := True;

         else
            Append (Self.U64, 0, Self.Integral_Count, Digit);
         end if;
      end if;
   end Append_Integral;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : out Packed_Decimal) is
   begin
      Self.Integral_Count      := 0;
      Self.Fractional_Offset   := U64_Limb_Offset'Last;
      Self.Fractional_Count    := 0;
      Self.Last_Non_Zero       := 0;
      Self.Exponent_Adjustment := 0;
      Self.Truncated           := False;
   end Clear;

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Limb  : Interfaces.Unsigned_64;
      Count : Interfaces.Unsigned_32;
      Value : out Interfaces.Unsigned_64)
   is
      Mask_1 : constant Interfaces.Unsigned_64 := 16#0F0F_0F0F_0F0F_0F0F#;
      Mask_2 : constant Interfaces.Unsigned_64 := 16#007F_007F_007F_007F#;
      Mask_3 : constant Interfaces.Unsigned_64 := 16#0000_3FFF_0000_3FFF#;
      Mask_4 : constant Interfaces.Unsigned_64 := 16#0000_0000_07FF_FFFF#;

   begin
      Value :=
        Interfaces.Shift_Left
          (Limb, Natural (64 - Count * Decimal_Digit'Value_Size));

      Value :=
        (Value and Mask_1) * 10
           + (Interfaces.Shift_Right (Value, 4) and Mask_1);
      Value :=
        (Value and Mask_2) * 100
           + (Interfaces.Shift_Right (Value, 8) and Mask_2);
      Value :=
        (Value and Mask_3) * 10_000
           + (Interfaces.Shift_Right (Value, 16) and Mask_3);
      Value :=
        (Value and Mask_4) * 100_000_000
           + (Interfaces.Shift_Right (Value, 32) and Mask_4);
   end Decode;

   ---------------------------
   -- Decode_As_Big_Integer --
   ---------------------------

   procedure Decode_As_Big_Integer
     (Self         : Packed_Decimal;
      Exponent     : Interfaces.Integer_32;
      Big_Mantissa : out VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Big_Exponent : out Interfaces.Integer_32)
   is
      Max_Decimal_Digits : constant := 769;
      --  Maximum number of significant digits of the mantissa to be read.
      --  Maximal number of exact digits of 64bit IEEE float is 768 and one
      --  more digit is used for rounding.

      U64_Limb_Decimal_Digits : constant := 16;

      Total_Digits : constant Interfaces.Unsigned_32 :=
        Self.Integral_Count + Self.Last_Non_Zero;
      Aux          : Interfaces.Unsigned_64;
      Digit_Count  : Interfaces.Unsigned_32 := 0;
      Step_Digits  : Interfaces.Unsigned_32;
      Limb_Index   : U64_Limb_Offset := 0;

   begin
      Big_Exponent :=
        Exponent
          + Interfaces.Integer_32
             (Interfaces.Unsigned_32'Min (Total_Digits, 19));

      while Digit_Count < Max_Decimal_Digits
        and Digit_Count < Self.Integral_Count
      loop
         Step_Digits :=
           Interfaces.Unsigned_32'Min
             (U64_Limb_Decimal_Digits,
              Interfaces.Unsigned_32'Min
                (Max_Decimal_Digits - Digit_Count,
                 Self.Integral_Count - Digit_Count));

         Decode (Self.U64 (Limb_Index), Step_Digits, Aux);

         Digit_Count := @ + Step_Digits;
         Limb_Index  := @ + 1;

         if Digit_Count = Max_Decimal_Digits then
            raise Program_Error;

         else
            Add (Big_Mantissa, Power_Of_10 (Step_Digits), Aux);
         end if;
      end loop;

      pragma Assert
        (Self.Fractional_Offset = U64_Limb_Offset'Last
           or Self.Fractional_Offset = Limb_Index);

      if Self.Fractional_Offset = U64_Limb_Offset'Last
        or Self.Last_Non_Zero = 0
      then
         Big_Exponent := @ - Interfaces.Integer_32 (Digit_Count);

         --  No fractional part or fractional part contains zeros only.

         return;
      end if;

      while Digit_Count < Max_Decimal_Digits
        and Digit_Count < Total_Digits
      loop
         Step_Digits :=
           Interfaces.Unsigned_32'Min
             (U64_Limb_Decimal_Digits,
              Interfaces.Unsigned_32'Min
                (Max_Decimal_Digits - Digit_Count,
                 Total_Digits - Digit_Count));

         Decode (Self.U64 (Limb_Index), Step_Digits, Aux);

         Digit_Count := @ + Step_Digits;
         Limb_Index  := @ + 1;

         if Digit_Count = Max_Decimal_Digits then
            raise Program_Error;

         else
            Add (Big_Mantissa, Power_Of_10 (Step_Digits), Aux);
         end if;
      end loop;

      Big_Exponent := @ - Interfaces.Integer_32 (Digit_Count);
   end Decode_As_Big_Integer;

   -----------------------
   -- Decode_As_Integer --
   -----------------------

   procedure Decode_As_Integer
     (Self             : Packed_Decimal;
      Significand      : out Interfaces.Unsigned_64;
      Power_Adjustment : out Interfaces.Integer_32;
      Truncated        : out Boolean)
   is
      Max_Fast_Decimal_Digits : constant := 19;
      --  Maximum number of significant digits of the mantissa to be read.
      --  Unsigned_64 is able to represent this number of digits. It is
      --  enough to represent absolute value of Integer_64 too. Number of
      --  digits of IEEE_Float_64 is less than this number.

      U64_Limb_Decimal_Digits : constant := 16;

      Total_Digits : constant Interfaces.Unsigned_32 :=
        Self.Integral_Count + Self.Last_Non_Zero;
      Aux          : Interfaces.Unsigned_64;
      Digit_Count  : Interfaces.Unsigned_32 := 0;
      Step_Digits  : Interfaces.Unsigned_32;
      Limb_Index   : U64_Limb_Offset := 0;

   begin
      Significand      := 0;
      Power_Adjustment := Self.Exponent_Adjustment;

      if Total_Digits = 0 then
         Truncated := False;

         return;
      end if;

      while Digit_Count < Max_Fast_Decimal_Digits
        and Digit_Count < Self.Integral_Count
      loop
         Step_Digits :=
           Interfaces.Unsigned_32'Min
             (U64_Limb_Decimal_Digits,
              Interfaces.Unsigned_32'Min
                (Max_Fast_Decimal_Digits - Digit_Count,
                 Self.Integral_Count - Digit_Count));

         Decode (Self.U64 (Limb_Index), Step_Digits, Aux);

         Significand := @ * Power_Of_10 (Step_Digits) + Aux;
         Digit_Count := @ + Step_Digits;
         Limb_Index  := @ + 1;
      end loop;

      if Digit_Count < Self.Integral_Count then
         Power_Adjustment :=
           Interfaces.Integer_32 (Self.Integral_Count - Digit_Count);
         Truncated        := True;

         return;
      end if;

      pragma Assert
        (Self.Fractional_Offset = U64_Limb_Offset'Last
           or Self.Fractional_Offset = Limb_Index);

      while Digit_Count < Max_Fast_Decimal_Digits
        and Digit_Count < Total_Digits
      loop
         Step_Digits :=
           Interfaces.Unsigned_32'Min
             (U64_Limb_Decimal_Digits,
              Interfaces.Unsigned_32'Min
                (Max_Fast_Decimal_Digits - Digit_Count,
                 Total_Digits - Digit_Count));

         Decode (Self.U64 (Limb_Index), Step_Digits, Aux);

         Significand      := @ * Power_Of_10 (Step_Digits) + Aux;
         Digit_Count      := @ + Step_Digits;
         Limb_Index       := @ + 1;
         Power_Adjustment := @ - Interfaces.Integer_32 (Step_Digits);
      end loop;

      Truncated := Digit_Count < Total_Digits;
   end Decode_As_Integer;

end VSS.JSON.Implementation.Packed_Decimals;
