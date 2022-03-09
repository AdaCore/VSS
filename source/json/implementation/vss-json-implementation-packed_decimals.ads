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
