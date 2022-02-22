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

with VSS.JSON.Implementation.Numbers.Clinger;
with VSS.JSON.Implementation.Numbers.Counters;
with VSS.JSON.Implementation.Numbers.Eisel_Lemire;
with VSS.Strings.Conversions;

package body VSS.JSON.Implementation.Numbers is

   use type Interfaces.Integer_32;
   use type Interfaces.Integer_64;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type VSS.Unicode.Code_Point;

   Digit_Zero : constant VSS.Unicode.Code_Point := 16#00_0030#;

   Decimal_Mantissa_Digits_Max : constant := 19;
   --  Maximum number of significant digits of the mantissa to be read.
   --  Unsigned_64 is able to represent this number of digits. It is enought
   --  to represent absolute value of Integer_64 too. Number of digits of
   --  IEEE_Float_64 is less than this number.

   Decimal_Exponent_Digits_Max : constant := 9;
   --  Maximum number of digits of the exponent.

   --  Decimal_Digits_Limit : constant := 50_000;
   --  --  Limit of decimal digits to avoid integer overflow of exponent.

   ---------------
   -- Exp_Digit --
   ---------------

   procedure Exp_Digit
     (Self  : in out Parsing_State;
      Digit : VSS.Unicode.Code_Point) is
   begin
      if Self.Error /= Not_A_Error then
         return;
      end if;

      Self.Has_Exponent := True;

      if Self.Exp_Value = 0 and Digit = Digit_Zero then
         --  Leading digit zero, nothing to do

         null;

      elsif Self.Collected_Exponent_Digits < Decimal_Exponent_Digits_Max then
         Self.Exp_Value :=
           Self.Exp_Value * 10 + Interfaces.Integer_32 (Digit - Digit_Zero);
         Self.Collected_Exponent_Digits := Self.Collected_Exponent_Digits + 1;

      else
         Self.Error := Out_Of_Range;
      end if;
   end Exp_Digit;

   ----------------
   -- Frac_Digit --
   ----------------

   procedure Frac_Digit
     (Self  : in out Parsing_State;
      Digit : VSS.Unicode.Code_Point) is
   begin
      if Self.Error /= Not_A_Error then
         return;
      end if;

      Self.Has_Fractional := True;

      if Self.Significand = 0 and Digit = Digit_Zero then
         --  Mantissa is zero and digit is zero too, adjust exponent

         Self.Exponent_Adjustment := Self.Exponent_Adjustment - 1;

      elsif Self.Collected_Mantissa_Digits < Decimal_Mantissa_Digits_Max then
         Self.Significand :=
           Self.Significand * 10 + Interfaces.Unsigned_64 (Digit - Digit_Zero);
         Self.Exponent_Adjustment := Self.Exponent_Adjustment - 1;
         Self.Collected_Mantissa_Digits := Self.Collected_Mantissa_Digits + 1;

      else
         Self.Mantissa_Is_Inexact :=
           Self.Mantissa_Is_Inexact or (Digit /= Digit_Zero);
         --  Self.Parsed_Digits := Self.Parsed_Digits + 1;

         --  if Self.Parsed_Digits >= Decimal_Digits_Limit then
         --     --  Stop processing of unreasonably long numbers.
         --
         --     Self.Error := Out_Of_Range;
         --  end if;

      --  else
      end if;
      --
      --  Self.Parsed_Digits := Self.Parsed_Digits + 1;
   end Frac_Digit;

   ---------------
   -- Int_Digit --
   ---------------

   procedure Int_Digit
     (Self  : in out Parsing_State;
      Digit : VSS.Unicode.Code_Point) is
   begin
      if Self.Error /= Not_A_Error then
         return;
      end if;

      if Self.Significand = 0 and Digit = Digit_Zero then
         --  Leading digit zero, nothing to do.

         null;

      elsif Self.Collected_Mantissa_Digits < Decimal_Mantissa_Digits_Max then
         Self.Significand                  :=
           Self.Significand * 10 + Interfaces.Unsigned_64 (Digit - Digit_Zero);
         Self.Collected_Mantissa_Digits :=
           Self.Collected_Mantissa_Digits + 1;

      else
         Self.Mantissa_Is_Inexact :=
           Self.Mantissa_Is_Inexact or (Digit /= Digit_Zero);
         Self.Exponent_Adjustment := Self.Exponent_Adjustment + 1;

         --  Self.Parsed_Digits := Self.Parsed_Digits + 1;

         --  if Self.Parsed_Digits >= Decimal_Digits_Limit then
         --     --  Stop processing of unreasonably long numbers.
         --
         --     Self.Error := Out_Of_Range;
         --  end if;
      end if;
   end Int_Digit;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Self : Parsing_State) return Boolean is
   begin
      return
        Self.Exponent_Adjustment = 0
          and Self.Exp_Value = 0
          --  and not Self.Has_Exponent
          and not Self.Mantissa_Is_Inexact;
   end Is_Integer;

   --------------------
   -- To_JSON_Number --
   --------------------

   procedure To_JSON_Number
     (Self         : Parsing_State;
      String_Value : VSS.Strings.Virtual_String;
      To           : out VSS.JSON.JSON_Number)
   is
      use type Interfaces.IEEE_Float_64;

      Max_Positive : constant Interfaces.Unsigned_64 :=
        Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
      --  Maximum absolute value of the maximum positive integer.

      Max_Negative : constant Interfaces.Unsigned_64 :=
        Interfaces.Unsigned_64 (-Interfaces.Integer_64'First);
      --  Maximum absolute value of the minimum negative integer.

      Exponent     : constant Interfaces.Integer_32 :=
        (if Self.Exp_Minus then -Self.Exp_Value else Self.Exp_Value)
        + Self.Exponent_Adjustment;

   begin
      if Self.Error /= Not_A_Error then
         To := (Out_Of_Range, String_Value);

         return;
      end if;

      --  Integer literals

      if not Self.Has_Fractional and not Self.Has_Exponent then
         --  Number has format of the integer value

         if Self.Mantissa_Is_Inexact or Exponent /= 0 then
            To := (Out_Of_Range, String_Value);

            return;
         end if;

         if Self.Minus then
            if Self.Significand > Max_Negative then
               To := (Out_Of_Range, String_Value);

            elsif Self.Significand = Max_Negative then
               To := (JSON_Integer, String_Value, Interfaces.Integer_64'First);

            else
               To :=
                 (JSON_Integer,
                  String_Value,
                  -Interfaces.Integer_64 (Self.Significand));
            end if;

         else
            if Self.Significand > Max_Positive then
               To := (Out_Of_Range, String_Value);

            else
               To :=
                 (JSON_Integer,
                  String_Value,
                  Interfaces.Integer_64 (Self.Significand));
            end if;
         end if;

         return;
      end if;

      --  Float literal

      if Exponent < -342 then
         --  Exponent is too small, result is zero

         To :=
           (JSON_Float, String_Value, (if Self.Minus then -0.0 else 0.0));

         return;

      elsif Exponent > 308 then
         --  Exponent is too large, result is out-of-range

         To := (Out_Of_Range, String_Value);

         return;

      else
         if Self.Significand = 0 then
            --  It is important to check significand to zero before main
            --  algorithm to avoid additional checks. Significand equal to
            --  zero is always exact, because all leading zeros are ignored
            --  during parsing.

            To :=
              (JSON_Float, String_Value, (if Self.Minus then -0.0 else 0.0));

            return;
         end if;

         declare
            Number     : Interfaces.IEEE_Float_64;
            Number_Aux : Interfaces.IEEE_Float_64;
            Success    : Boolean := False;

         begin
            if not Self.Mantissa_Is_Inexact then
               --  If significant is exact number attempt to convert it by
               --  fastest algoriphm.

               Clinger.Convert (Self.Significand, Exponent, Number, Success);
            end if;

            if not Success then
               Eisel_Lemire.Convert
                 (Self.Significand, Exponent, Number, Success);

               if Success and Self.Mantissa_Is_Inexact then
                  --  When significan is not exact try to compute value for
                  --  the next value of significand.

                  Eisel_Lemire.Convert
                    (Self.Significand + 1, Exponent, Number_Aux, Success);

                  --  If computed value is not equal to first one, more
                  --  complicated conversion need to be used, thus fail.
                  --  Infinity values are invalid in Ada, thus need to be
                  --  checked separately, otherwise Constraint_Error is
                  --  raised in data validity checking mode.

                  if (Number'Valid or Number_Aux'Valid)
                    and (not Number'Valid
                           or else not Number_Aux'Valid
                           or else Number /= Number_Aux)
                  then
                     Success := False;
                  end if;
               end if;
            end if;

            if Success then
               if Number'Valid then
                  To :=
                    (JSON_Float,
                     String_Value,
                     (if Self.Minus then -Number else Number));

               else
                  To := (Out_Of_Range, String_Value);
               end if;

               Success := True;

               return;
            end if;
         end;
      end if;

      --  Fallback to use of 'Value attribute

      declare
         Image : constant String :=
           VSS.Strings.Conversions.To_UTF_8_String (String_Value);
         Value : constant Interfaces.IEEE_Float_64 :=
           Interfaces.IEEE_Float_64'Value (Image);

      begin
         Counters.Standard_Value_Total := Counters.Standard_Value_Total + 1;

         if Value'Valid then
            To := (JSON_Float, String_Value, Value);

         else
            To := (Out_Of_Range, String_Value);
         end if;
      end;
   end To_JSON_Number;

end VSS.JSON.Implementation.Numbers;
