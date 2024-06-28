--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

package body VSS.JSON.Implementation.Arithmetic_64
  with Preelaborate
is

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

   ------------------
   -- Multiply_Add --
   ------------------

   procedure Multiply_Add
     (Left     : Interfaces.Unsigned_64;
      Right    : Interfaces.Unsigned_64;
      Result   : out Interfaces.Unsigned_64;
      Overflow : in out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_128;

      R : constant Interfaces.Unsigned_128 :=
        Interfaces.Unsigned_128 (Left) * Interfaces.Unsigned_128 (Right)
          + Interfaces.Unsigned_128 (Overflow);

   begin
      Result   := Interfaces.Unsigned_64 (R mod 2 ** 64);
      Overflow := Interfaces.Unsigned_64 (R / 2 ** 64);
   end Multiply_Add;

end VSS.JSON.Implementation.Arithmetic_64;
