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
      use type Interfaces.Unsigned_64;

      AL : constant Interfaces.Unsigned_64 := A and 16#FFFF_FFFF#;
      AH : constant Interfaces.Unsigned_64 := Interfaces.Shift_Right (A, 32);
      BL : constant Interfaces.Unsigned_64 := B and 16#FFFF_FFFF#;
      BH : constant Interfaces.Unsigned_64 := Interfaces.Shift_Right (B, 32);

      U  : constant Interfaces.Unsigned_64 := AL * BL;
      T  : constant Interfaces.Unsigned_64 :=
        AH * BL + Interfaces.Shift_Right (U, 32);
      TL : constant Interfaces.Unsigned_64 := T and 16#FFFF_FFFF#;
      W1 : constant Interfaces.Unsigned_64 := TL + AL * BH;
      W2 : constant Interfaces.Unsigned_64 := Interfaces.Shift_Right (T, 32);

   begin
      L := A * B;
      H := AH * BH + W2 + Interfaces.Shift_Right (W1, 32);
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
      use type Interfaces.Unsigned_64;

      C : constant Interfaces.Unsigned_64 := Overflow;
      L : Interfaces.Unsigned_64;

   begin
      Multiply (Left, Right, L, Overflow);

      Result := L + C;

      if Result < L or Result < C then
         Overflow := @ + 1;
      end if;
   end Multiply_Add;

end VSS.JSON.Implementation.Arithmetic_64;
