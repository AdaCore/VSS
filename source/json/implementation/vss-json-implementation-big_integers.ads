--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Integers with arbitrary precision
--
--  Implements a small subset of big integer arithmetic, using simple
--  algorithms since asymptotically faster algorithms are slower for a small
--  number of limbs. All operations assume the big-integer is normalized.

with Interfaces;

package VSS.JSON.Implementation.Big_Integers is

   pragma Preelaborate;

   type Compare_Kind is (Less, Equal, Greater);

   type Big_Integer is private;

   procedure Set
     (Self : in out Big_Integer; Value : Interfaces.Unsigned_64);

   function Compare
     (Self  : Big_Integer;
      Other : Big_Integer) return Compare_Kind;
   --  Compare two big integers.

   procedure Add
     (Self  : in out Big_Integer;
      Value : Interfaces.Unsigned_64);
   --  Add scalar value to bigint.

   procedure Multiply
     (Self  : in out Big_Integer;
      Value : Interfaces.Unsigned_64);

   procedure Multiply_Power_2
     (Self : in out Big_Integer; Exponent : Interfaces.Integer_32);
   --  Multiply as if by 2 raised to a power.

   procedure Multiply_Power_5
     (Self : in out Big_Integer; Exponent : Interfaces.Integer_32);
   --  Multiply as if by 5 raised to a power.

   procedure Multiply_Power_10
     (Self : in out Big_Integer; Exponent : Interfaces.Integer_32);
   --  Multiply as if by 10 raised to a power.

   procedure Get_High_64
     (Self   : Big_Integer;
      Value  : out Interfaces.Unsigned_64;
      Truncated : out Boolean);
   --  Get the high 64 bits from the vector, and if bits were truncated.
   --  This is to get the significant digits for the float.

   function Size (Self : Big_Integer) return Interfaces.Integer_32;
   --  Get the number of bits in the bigint.

   function Non_Zero
     (Self : Big_Integer; From : Interfaces.Integer_32) return Boolean;
   --  Check if any limbs are non-zero after the given index. This needs to
   --  be done in reverse order, since the index is relative to the most
   --  significant limbs.

private

   use type Interfaces.Integer_32;

   subtype Limb_Type is Interfaces.Unsigned_64;

   type Limb_Array is
     array (Interfaces.Integer_32 range <>) of aliased Limb_Type;

   subtype Big_Integer_Limb_Array is Limb_Array (0 .. 249);

   type Big_Integer is record
      Data : Big_Integer_Limb_Array;
      Last : Interfaces.Integer_32 := -1;
   end record;

end VSS.JSON.Implementation.Big_Integers;
