--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
            --  bits and exponent is in 22 + 16, then value can be
            --  constructed by two multiplications.
            --
            --  XXX Not implemented;
         end;
      end if;

      Success := False;
   end Convert;

end VSS.JSON.Implementation.Numbers.Clinger;
