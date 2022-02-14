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

package body VSS.JSON.Implementation.Numbers is

   use type Interfaces.Unsigned_64;

   Digit_Zero : constant VSS.Unicode.Code_Point := 16#00_0030#;

   procedure Append_Digit
     (Value : in out Interfaces.Unsigned_64;
      Digit : VSS.Unicode.Code_Point;
      Error : in out Parsing_Error_States);

   ------------------
   -- Append_Digit --
   ------------------

   procedure Append_Digit
     (Value : in out Interfaces.Unsigned_64;
      Digit : VSS.Unicode.Code_Point;
      Error : in out Parsing_Error_States)
   is
      use type VSS.Unicode.Code_Point;

      Unsigned_Max_10 : constant Interfaces.Unsigned_64 :=
        Interfaces.Unsigned_64'Last / 10;
      --  Numbers bigger than this value overflow if multiplied by 10

      Digit_Value     : constant Interfaces.Unsigned_64 :=
        Interfaces.Unsigned_64 (Digit - Digit_Zero);

   begin
      if Error /= Not_A_Error then
         return;
      end if;

      if Value >= Unsigned_Max_10 then
         Error := Out_Of_Range;

      else
         Value := 10 * Value + Digit_Value;
      end if;
   end Append_Digit;

   ---------------
   -- Int_Digit --
   ---------------

   procedure Int_Digit
     (Self  : in out Parsing_State;
      Digit : VSS.Unicode.Code_Point) is
   begin
      Append_Digit (Self.Int_Value, Digit, Self.Error);
   end Int_Digit;

   --------------------
   -- To_JSON_Number --
   --------------------

   procedure To_JSON_Number
     (Self         : Parsing_State;
      String_Value : VSS.Strings.Virtual_String;
      To           : out VSS.JSON.JSON_Number)
   is
      use type Interfaces.Integer_64;

      Max_Positive : constant Interfaces.Unsigned_64 :=
        Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
      --  Maximum absolute value of the maximum positive integer.

      Max_Negative : constant Interfaces.Unsigned_64 :=
        Interfaces.Unsigned_64 (-Interfaces.Integer_64'First);
      --  Maximum absolute value of the minimum negative integer.

   begin
      if Self.Error /= Not_A_Error then
         To := (Out_Of_Range, String_Value);

      else
         if Self.Exp_Value /= 0 or else Self.Frac_Value /= 0 then
            raise Program_Error;
         end if;

         if Self.Minus then
            if Self.Int_Value > Max_Negative then
               To := (Out_Of_Range, String_Value);

            elsif Self.Int_Value = Max_Negative then
               To := (JSON_Integer, String_Value, Interfaces.Integer_64'First);

            else
               To :=
                 (JSON_Integer,
                  String_Value,
                  -Interfaces.Integer_64 (Self.Int_Value));
            end if;

         else
            if Self.Int_Value > Max_Positive then
               To := (Out_Of_Range, String_Value);

            else
               To :=
                 (JSON_Integer,
                  String_Value,
                  Interfaces.Integer_64 (Self.Int_Value));
            end if;
         end if;
      end if;
   end To_JSON_Number;

end VSS.JSON.Implementation.Numbers;
