--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.JSON.Implementation.Big_Integers;
with VSS.JSON.Implementation.Numbers.Clinger;
with VSS.JSON.Implementation.Numbers.Eisel_Lemire;

package body VSS.JSON.Implementation.Numbers is

   use type Interfaces.Integer_32;
   use type Interfaces.Integer_64;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type VSS.Unicode.Code_Point;

   Digit_Zero : constant VSS.Unicode.Code_Point := 16#00_0030#;

   Decimal_Exponent_Digits_Max : constant := 9;
   --  Maximum number of digits of the exponent.

   --  Decimal_Digits_Limit : constant := 50_000;
   --  --  Limit of decimal digits to avoid integer overflow of exponent.

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left : Decoded_Float; Right : Decoded_Float) return Boolean is
   begin
      return
        Left.Significand = Right.Significand
          and then Left.Power = Right.Power;
   end "=";

   -------------------
   -- Decimal_Point --
   -------------------

   procedure Decimal_Point (Self : in out Parsing_State) is
   begin
      Self.Has_Fractional := True;

      VSS.JSON.Implementation.Packed_Decimals.Append_Decimal_Point
        (Self.Decimal);
   end Decimal_Point;

   -----------------------
   -- Encode_IEEE_Float --
   -----------------------

   procedure Encode_IEEE_Float
     (Decoded  : Decoded_Float;
      Negative : Boolean;
      Encoded  : out Interfaces.IEEE_Float_64)
   is
      N_U64 : Interfaces.Unsigned_64 with Address => Encoded'Address;
      --  This subprogram should be able to process Inf values, which is not
      --  valid value of floating point type in Ada, thus exception is raised
      --  in validity checks mode. To prevent this overlapped variable is used.

   begin
      N_U64 :=
        Decoded.Significand
          or Interfaces.Shift_Left
              (Interfaces.Unsigned_64 (Decoded.Power),
               Eisel_Lemire.Mantissa_Explicit_Bits);

      if Negative then
         N_U64 := @ or 16#8000_0000_0000_0000#;
      end if;
   end Encode_IEEE_Float;

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

      VSS.JSON.Implementation.Packed_Decimals.Append_Fractional
        (Self.Decimal,
         VSS.JSON.Implementation.Packed_Decimals.Decimal_Digit
           (Digit - Digit_Zero));
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

      VSS.JSON.Implementation.Packed_Decimals.Append_Integral
        (Self.Decimal,
         VSS.JSON.Implementation.Packed_Decimals.Decimal_Digit
           (Digit - Digit_Zero));
   end Int_Digit;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Parsing_State) is
   begin
      Self.Error                     := Not_A_Error;
      Self.Minus                     := False;
      Self.Has_Fractional            := False;
      Self.Has_Exponent              := False;
      Self.Exp_Minus                 := False;
      Self.Exp_Value                 := 0;
      Self.Collected_Exponent_Digits := 0;

      VSS.JSON.Implementation.Packed_Decimals.Clear (Self.Decimal);
   end Reset;

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

      Mantissa     : Interfaces.Unsigned_64;
      Exponent     : Interfaces.Integer_32;
      Inexact      : Boolean;

   begin
      if Self.Error /= Not_A_Error then
         To := (Out_Of_Range, String_Value);

         return;
      end if;

      VSS.JSON.Implementation.Packed_Decimals.Decode_As_Integer
        (Self.Decimal, Mantissa, Exponent, Inexact);

      Exponent :=
        @ + (if Self.Exp_Minus then -Self.Exp_Value else Self.Exp_Value);

      --  Integer literals

      if not Self.Has_Fractional and not Self.Has_Exponent then
         --  Number has format of the integer value

         if Inexact or Exponent /= 0 then
            To := (Out_Of_Range, String_Value);

            return;
         end if;

         if Self.Minus then
            if Mantissa > Max_Negative then
               To := (Out_Of_Range, String_Value);

            elsif Mantissa = Max_Negative then
               To := (JSON_Integer, String_Value, Interfaces.Integer_64'First);

            else
               To :=
                 (JSON_Integer,
                  String_Value,
                  -Interfaces.Integer_64 (Mantissa));
            end if;

         else
            if Mantissa > Max_Positive then
               To := (Out_Of_Range, String_Value);

            else
               To :=
                 (JSON_Integer,
                  String_Value,
                  Interfaces.Integer_64 (Mantissa));
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
         if Mantissa = 0 then
            --  It is important to check significand to zero before main
            --  algorithm to avoid additional checks. Significand equal to
            --  zero is always exact, because all leading zeros are ignored
            --  during parsing.

            To :=
              (JSON_Float, String_Value, (if Self.Minus then -0.0 else 0.0));

            return;
         end if;

         declare
            Number     : Decoded_Float;
            Number_1   : Decoded_Float;
            Number_Aux : Interfaces.IEEE_Float_64;
            Success    : Boolean := False;

         begin
            if not Inexact then
               --  If significant is exact number attempt to convert it by
               --  fastest algorithm.

               Clinger.Convert (Mantissa, Exponent, Number_Aux, Success);

               if Success then
                  To :=
                    (JSON_Float,
                     String_Value,
                     (if Self.Minus then -Number_Aux else Number_Aux));

                  return;
               end if;
            end if;

            Eisel_Lemire.Convert (Mantissa, Exponent, Number, Success);

            if Success and Inexact then
               --  When significan is not exact try to compute value for
               --  the next value of significand.

               Eisel_Lemire.Convert
                 (Mantissa + 1, Exponent, Number_1, Success);

               --  If computed value is not equal to first one, more
               --  complicated conversion need to be used, thus fail.

               if Number /= Number_1 then
                  Eisel_Lemire.Compute_Error
                    (Mantissa,
                     Exponent,
                     Number);

                  Success := False;
               end if;
            end if;

            if Success then
               if Number.Power /= Eisel_Lemire.Infinite_Power then
                  Encode_IEEE_Float (Number, Self.Minus, Number_Aux);

                  To := (JSON_Float, String_Value, Number_Aux);

               else
                  To := (Out_Of_Range, String_Value);
               end if;

               Success := True;

               return;
            end if;

            declare
               Big_Mantissa : VSS.JSON.Implementation.Big_Integers.Big_Integer;
               Big_Exponent : Interfaces.Integer_32;
               Error        : constant Decoded_Float :=
                 (Significand => Number.Significand,
                  Power       => Number.Power - Eisel_Lemire.Invalid_Bias);

            begin
               VSS.JSON.Implementation.Packed_Decimals.Decode_As_Big_Integer
                 (Self.Decimal, Exponent, Big_Mantissa, Big_Exponent);

               if Big_Exponent >= 0 then
                  Eisel_Lemire.Scale_Positive
                    (Big_Mantissa, Big_Exponent, Number);

               else
                  Eisel_Lemire.Scale_Negative
                    (Big_Mantissa, Big_Exponent, Error, Number);
               end if;

            end;

            if Number.Power /= Eisel_Lemire.Infinite_Power then
               Encode_IEEE_Float (Number, Self.Minus, Number_Aux);

               To := (JSON_Float, String_Value, Number_Aux);

            else
               To := (Out_Of_Range, String_Value);
            end if;

            return;
         end;
      end if;
   end To_JSON_Number;

end VSS.JSON.Implementation.Numbers;
