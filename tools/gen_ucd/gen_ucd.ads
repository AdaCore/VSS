------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
   type Unsigned_6  is mod 2 ** 6  with Size => 7;

   type Unsigned_8  is mod 2 ** 8  with Size => 8;
   type Unsigned_9  is mod 2 ** 9  with Size => 9;

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
