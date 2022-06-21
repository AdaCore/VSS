--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package Gen_UCD is

   pragma Pure;

   --------------------
   -- Unsigned types --
   --------------------

   type Unsigned_1  is mod 2 ** 1  with Size => 1;
   type Unsigned_2  is mod 2 ** 2  with Size => 2;
   type Unsigned_3  is mod 2 ** 3  with Size => 3;
   type Unsigned_4  is mod 2 ** 4  with Size => 4;
   type Unsigned_5  is mod 2 ** 5  with Size => 5;
   type Unsigned_6  is mod 2 ** 6  with Size => 6;
   type Unsigned_7  is mod 2 ** 7  with Size => 7;
   type Unsigned_8  is mod 2 ** 8  with Size => 8;
   type Unsigned_9  is mod 2 ** 9  with Size => 9;
   type Unsigned_10 is mod 2 ** 10 with Size => 10;
   type Unsigned_11 is mod 2 ** 11 with Size => 11;

   type Unsigned_14 is mod 2 ** 14 with Size => 14;
   type Unsigned_15 is mod 2 ** 15 with Size => 15;
   type Unsigned_16 is mod 2 ** 16 with Size => 16;
   type Unsigned_17 is mod 2 ** 17 with Size => 17;

   type Unsigned_32 is mod 2 ** 32 with Size => 32;

   type Unsigned_64 is mod 2 ** 64 with Size => 64;

   --------------------------
   -- UTF-8 encoding types --
   --------------------------

   type UTF_8_Code_Unit is new Unsigned_8;

   type UTF_8_Offset is range -2 ** 15 .. 2 ** 15 - 1;
   subtype UTF_8_Count is UTF_8_Offset range 0 .. UTF_8_Offset'Last;

end Gen_UCD;
