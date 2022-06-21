--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Packed decimals

with Interfaces;

with VSS.JSON.Implementation.Big_Integers;

package VSS.JSON.Implementation.Packed_Decimals is

   pragma Preelaborate;

   Number_Of_Digits : constant := 784;
   --  Exact value of 64bit IEEE float can be up to 768 digits. One additional
   --  digit is necessary for rounding. Also, this value must be adjusted to
   --  internal limb representation which is 64bit.

   type Decimal_Digit is range 0 .. 9;

   type Packed_Decimal is private;

   procedure Clear (Self : out Packed_Decimal);

   procedure Append_Integral
     (Self  : in out Packed_Decimal;
      Digit : Decimal_Digit);

   procedure Append_Decimal_Point (Self  : in out Packed_Decimal);

   procedure Append_Fractional
     (Self  : in out Packed_Decimal;
      Digit : Decimal_Digit);

   procedure Decode_As_Integer
     (Self             : Packed_Decimal;
      Significand      : out Interfaces.Unsigned_64;
      Power_Adjustment : out Interfaces.Integer_32;
      Truncated        : out Boolean);

   procedure Decode_As_Big_Integer
     (Self         : Packed_Decimal;
      Exponent     : Interfaces.Integer_32;
      Big_Mantissa : out VSS.JSON.Implementation.Big_Integers.Big_Integer;
      Big_Exponent : out Interfaces.Integer_32);

private

   use type Interfaces.Unsigned_32;

   Number_Of_U64_Limbs : constant :=
     (Number_Of_Digits * Decimal_Digit'Value_Size)
        / Interfaces.Unsigned_64'Size + 1;
   --  Amount of 64-bit limbs to store packed decimal number of required
   --  size plus one limb to align fractional part at the boundary of limb.

   pragma Compile_Time_Error
     ((Number_Of_Digits * Decimal_Digit'Value_Size)
         mod Interfaces.Unsigned_64'Size /= 0,
      "number of digits should allow to declare array of digits that"
        & " may be processed as array Unsigned_64");

   type U64_Limb_Offset is new Interfaces.Unsigned_32;

   type Unsigned_64_Array is
     array (U64_Limb_Offset range 0 .. Number_Of_U64_Limbs - 1)
       of Interfaces.Unsigned_64;

   type Packed_Decimal is record
      U64                 : Unsigned_64_Array;
      Integral_Count      : Interfaces.Unsigned_32 := 0;
      Fractional_Offset   : U64_Limb_Offset        := U64_Limb_Offset'Last;
      Fractional_Count    : Interfaces.Unsigned_32 := 0;
      Last_Non_Zero       : Interfaces.Unsigned_32 := 0;
      Exponent_Adjustment : Interfaces.Integer_32  := 0;
      Truncated           : Boolean                := False;
   end record;

end VSS.JSON.Implementation.Packed_Decimals;
