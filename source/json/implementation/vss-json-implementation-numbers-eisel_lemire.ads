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
--  Conversion of a decimal 64-bit unsigned integer exact significand and
--  32-bit signed decimal exponent into 64-bit IEEE 754 floating point format.
--
--  Algorithm is described in paper https://arxiv.org/pdf/2101.11408.pdf,
--  blog post https://nigeltao.github.io/blog/2020/eisel-lemire.html, and
--  reference implementation https://github.com/fastfloat/fast_float

package VSS.JSON.Implementation.Numbers.Eisel_Lemire is

   pragma Preelaborate;

   use type Interfaces.Unsigned_64;

   Smallest_Power_Of_Ten      : constant := -342;     --  -65
   Largest_Power_Of_Ten       : constant := 308;      --  38
   Infinite_Power             : constant := 16#7FF#;  --  16#FF#
   Minimum_Exponent           : constant := -1023;    --  -127
   Mantissa_Explicit_Bits     : constant := 52;       --  23
   Min_Exponent_Round_To_Even : constant := -4;       --  -17
   Max_Exponent_Round_To_Even : constant := 23;       --  10;

   procedure Convert
     (Significand  : Interfaces.Unsigned_64;
      Exponent_10  : Interfaces.Integer_32;
      Number       : out Interfaces.IEEE_Float_64;
      Success      : out Boolean)
     with Pre =>
       Significand /= 0
         and Exponent_10 in Smallest_Power_Of_Ten .. Largest_Power_Of_Ten;
   --  Attempt to do conversion. On success, set Number parameter to result
   --  value is computed, and set Success parameter to True. Set Success
   --  parameter to False if algorithm fails for any reasons.

end VSS.JSON.Implementation.Numbers.Eisel_Lemire;
