--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Multiply and Multiply_Add opertations on Integer_64 that returns
--  unpacked Integer_128 result and can be implemented in hardware.

with Interfaces;

package VSS.JSON.Implementation.Arithmetic_64
  with Preelaborate
is

   procedure Multiply
     (A : Interfaces.Unsigned_64;
      B : Interfaces.Unsigned_64;
      L : out Interfaces.Unsigned_64;
      H : out Interfaces.Unsigned_64) with Inline;
   --  Multiplication of two 64-bit unsigned integers into 128-bit values,
   --  splitted into high and low 64-bit unsigned integers. On x86_64 it is
   --  optimized into single instruction.

   procedure Multiply_Add
     (Left     : Interfaces.Unsigned_64;
      Right    : Interfaces.Unsigned_64;
      Result   : out Interfaces.Unsigned_64;
      Overflow : in out Interfaces.Unsigned_64) with Inline;
   --  Multiplication of two 64-bit unsigned integers into 128-bit values,
   --  add of carry. Result is splitted into high and low 64-bit unsigned
   --  integers. On x86_64 it is optimized into few instructions.

end VSS.JSON.Implementation.Arithmetic_64;
