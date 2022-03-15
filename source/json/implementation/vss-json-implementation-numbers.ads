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
--  Utilities to read/write numbers.

with VSS.JSON.Implementation.Packed_Decimals;
with VSS.Unicode;

package VSS.JSON.Implementation.Numbers is

   pragma Preelaborate;

   type Parsing_Error_States is (Not_A_Error, Out_Of_Range, Precision_Lost);

   type Parsing_State is record
      Error                     : Parsing_Error_States   := Not_A_Error;
      Minus                     : Boolean                := False;
      Has_Fractional            : Boolean                := False;
      Has_Exponent              : Boolean                := False;
      Exp_Minus                 : Boolean                := False;
      Exp_Value                 : Interfaces.Integer_32  := 0;
      Collected_Exponent_Digits : Interfaces.Unsigned_32 := 0;
      Decimal                   :
        VSS.JSON.Implementation.Packed_Decimals.Packed_Decimal;
   end record;

   procedure Reset (Self : in out Parsing_State);
   --  Reset state to initial.

   procedure Int_Digit
     (Self  : in out Parsing_State;
      Digit : VSS.Unicode.Code_Point);
   --  Process next digit of 'int' expression. Digit must be valid character
   --  inside '0' .. '9' range.

   procedure Decimal_Point (Self : in out Parsing_State);
   --  Process decimal point between integral and fractional parts of the
   --  number.

   procedure Frac_Digit
     (Self  : in out Parsing_State;
      Digit : VSS.Unicode.Code_Point);
   --  Process next digit of 'frac' expression. Digit must be valid character
   --  inside '0' .. '9' range.

   procedure Exp_Digit
     (Self  : in out Parsing_State;
      Digit : VSS.Unicode.Code_Point);
   --  Process next digit of 'exp' expression. Digit must be valid character
   --  inside '0' .. '9' range.

   procedure To_JSON_Number
     (Self         : Parsing_State;
      String_Value : VSS.Strings.Virtual_String;
      To           : out VSS.JSON.JSON_Number);
   --  Converts parsed value to JSON_Number and set it to To parameter.

private

   type Decoded_Float is record
      Significand : Interfaces.Unsigned_64 := 0;
      Power       : Interfaces.Integer_32  := 0;
   end record;

   overriding function "="
     (Left : Decoded_Float; Right : Decoded_Float) return Boolean;

   procedure Encode_IEEE_Float
     (Decoded  : Decoded_Float;
      Negative : Boolean;
      Encoded  : out Interfaces.IEEE_Float_64);

end VSS.JSON.Implementation.Numbers;
