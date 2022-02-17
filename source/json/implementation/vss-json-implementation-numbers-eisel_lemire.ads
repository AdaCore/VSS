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
--  Algorithm is described in paper https://arxiv.org/pdf/2101.11408.pdf
--  and blog post https://nigeltao.github.io/blog/2020/eisel-lemire.html
--
--  Most comments in the code are taken from https://github.com/google/wuffs
--  as mostly descriptive implementation.

package VSS.JSON.Implementation.Numbers.Eisel_Lemire is

   pragma Preelaborate;

   use type Interfaces.Unsigned_64;

   procedure Convert
     (Significand  : Interfaces.Unsigned_64;
      Exponent_10  : Interfaces.Integer_32;
      Number       : out Interfaces.IEEE_Float_64;
      Success      : out Boolean)
     with Pre => Significand /= 0;
   --  Attempt to do conversion. On success, set Number parameter to result
   --  value is computed, and set Out_Of_Range parameter to True when value
   --  has been computed but outside of 64-bit floating point range. Set
   --  Success parameter to False if algorithm fails for any reasons.

end VSS.JSON.Implementation.Numbers.Eisel_Lemire;
