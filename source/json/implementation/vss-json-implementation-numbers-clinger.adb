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

with VSS.JSON.Implementation.Numbers.Tables;

package body VSS.JSON.Implementation.Numbers.Clinger is

   -------------
   -- Convert --
   -------------

   procedure Convert
     (Significand  : Interfaces.Unsigned_64;
      Exponent_10  : Interfaces.Integer_32;
      Number       : out Interfaces.IEEE_Float_64;
      Success      : out Boolean)
   is
      use type Interfaces.IEEE_Float_64;
      use type Interfaces.Integer_32;
      use type Interfaces.Unsigned_64;

   begin
      if Significand < 2 ** 53 then  --  9007199254740992
         declare
            Aux : constant Interfaces.IEEE_Float_64 :=
              Interfaces.IEEE_Float_64 (Significand);

         begin
            --  Exponent and significand * exponent can be represented
            --  as 64-bit floating point value.

            if Exponent_10 in -Tables.Exact_Powers_Of_10'Last .. -1 then
               Number  := Aux / Tables.Exact_Powers_Of_10 (-Exponent_10);
               Success := True;

               return;
            end if;

            --  Exponent and significand * exponent can be represented
            --  as 64-bit floating point value.

            if Exponent_10 in 0 .. Tables.Exact_Powers_Of_10'Last then
               Number  := Aux * Tables.Exact_Powers_Of_10 (Exponent_10);
               Success := True;

               return;
            end if;

            --  There is the case when significand has enough unused
            --  bits and exponent is in 22 + 16, then valur can be
            --  constructed by two multiplications.
            --
            --  XXX Not implemented;
         end;
      end if;

      Success := False;
   end Convert;

end VSS.JSON.Implementation.Numbers.Clinger;
