--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Fixed.Hash;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with VSS.JSON.Implementation.Numbers;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_JSON_Decimal_To_Number is

   use type Interfaces.IEEE_Float_64;

   procedure Run_Test (File_Name : String);

   package Unsigned_64_IO is
     new Ada.Text_IO.Modular_IO (Interfaces.Unsigned_64);

   function To_Unsigned_64 is
     new Ada.Unchecked_Conversion
           (Interfaces.IEEE_Float_64, Interfaces.Unsigned_64);

   package String_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
           (String, Ada.Strings.Fixed.Hash, "=");

   Exceptions : String_Sets.Set;

   procedure Load_Exceptions (File_Name : String);

   procedure Process_String
     (State  : in out VSS.JSON.Implementation.Numbers.Parsing_State;
      Image  : String);

   T : Natural := 0;

   ---------------------
   -- Load_Exceptions --
   ---------------------

   procedure Load_Exceptions (File_Name : String) is
      File      : Ada.Text_IO.File_Type;
      Buffer    : String (1 .. 4096);
      Last      : Natural;

   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Buffer, Last);

         if Last >= Buffer'First then
            Exceptions.Include (Buffer (Buffer'First .. Last));
         end if;
      end loop;
   end Load_Exceptions;

   --------------------
   -- Process_String --
   --------------------

   procedure Process_String
     (State  : in out VSS.JSON.Implementation.Numbers.Parsing_State;
      Image : String)
   is
      type Modes is (Initial, Int, Frac, Exp);

      Mode  : Modes := Initial;
      Index : Positive := Image'First;
      C     : Character;

   begin
      T := T + 1;

      while Index <= Image'Last loop
         C := Image (Index);

         case Mode is
            when Initial =>
               if C in '0' .. '9' then
                  VSS.JSON.Implementation.Numbers.Int_Digit
                    (State, Character'Pos (C));
                  Mode := Int;

               elsif C = '.' then
                  VSS.JSON.Implementation.Numbers.Decimal_Point (State);
                  Mode := Frac;

               else
                  raise Program_Error
                    with "Unexpected character '"
                            & C & "' in " & Modes'Image (Mode);
               end if;

            when Int =>
               if C in '0' .. '9' then
                  VSS.JSON.Implementation.Numbers.Int_Digit
                    (State, Character'Pos (C));

               elsif C = '.' then
                  VSS.JSON.Implementation.Numbers.Decimal_Point (State);
                  Mode := Frac;

               elsif C in 'e' | 'E' then
                  Mode := Exp;

               else
                  raise Program_Error
                    with "Unexpected character '"
                            & C & "' in " & Modes'Image (Mode);
               end if;

            when Frac =>
               if C in '0' .. '9' then
                  VSS.JSON.Implementation.Numbers.Frac_Digit
                    (State, Character'Pos (C));

               elsif C in 'e' | 'E' then
                  Mode := Exp;

               else
                  raise Program_Error
                    with "Unexpected character '"
                            & C & "' in " & Modes'Image (Mode);
               end if;

            when Exp =>
               if C in '0' .. '9' then
                  VSS.JSON.Implementation.Numbers.Exp_Digit
                    (State, Character'Pos (C));

               elsif C = '+' then
                  null;

               elsif C = '-' then
                  State.Exp_Minus := True;

               else
                  raise Program_Error
                    with "Unexpected character '"
                            & C & "' in " & Modes'Image (Mode);
               end if;
         end case;

         Index := Index + 1;
      end loop;
   end Process_String;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (File_Name : String) is
      use type VSS.JSON.JSON_Number_Kind;

      File      : Ada.Text_IO.File_Type;
      Buffer    : String (1 .. 4096);
      Last      : Natural;
      Separator : Positive;
      First     : Positive;
      Binary    : Interfaces.Unsigned_64;

   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Buffer, Last);

         if Last >= Buffer'First then
            Separator := Buffer'First;

            --  Skip representation of 16-bit float

            while Separator <= Last and then Buffer (Separator) /= ' ' loop
               Separator := Separator + 1;
            end loop;

            Separator := Separator + 1;

            --  Skip representation of 32-bit float

            while Separator <= Last and then Buffer (Separator) /= ' ' loop
               Separator := Separator + 1;
            end loop;

            Separator := Separator + 1;

            --  Read representation of 64-bit float

            First := Separator;

            while Separator <= Last and then Buffer (Separator) /= ' ' loop
               Separator := Separator + 1;
            end loop;

            Binary :=
              Interfaces.Unsigned_64'Value
                ("16#" & Buffer (First .. Separator - 1) & '#');

            Separator := Separator + 1;

            --  Read decimal representation of the value
            --
            --  Correct format is assumed

            if not Exceptions.Contains (Buffer (Separator .. Last)) then
               declare
                  use type Interfaces.Unsigned_64;

                  State : VSS.JSON.Implementation.Numbers.Parsing_State;
                  R     : VSS.JSON.JSON_Number;
                  B1    : String (1 .. 20);
                  B2    : String (1 .. 20);

               begin
                  First := Separator;

                  Process_String (State, Buffer (Separator .. Last));

                  --  Force float mode

                  State.Has_Fractional := True;

                  VSS.JSON.Implementation.Numbers.To_JSON_Number
                    (State,
                     VSS.Strings.Conversions.To_Virtual_String
                       (Buffer (First .. Last)),
                     R);

                  if Binary = 16#0000_0000_0000_0000#
                    and R.Kind = VSS.JSON.Out_Of_Range
                  then
                     --  Out_Of_Range means value is too small to be
                     --  represented as float.

                     null;

                  elsif Binary = 16#7FF0_0000_0000_0000# then
                     --  Special case for Inf

                     Test_Support.Assert (R.Kind = VSS.JSON.Out_Of_Range);

                  else
                     Unsigned_64_IO.Put (B1, Binary, 16);

                     if R.Kind /= VSS.JSON.Out_Of_Range then
                        Unsigned_64_IO.Put
                          (B2, To_Unsigned_64 (R.Float_Value), 16);

                     else
                        B2 := "Out-Of-Range        ";
                     end if;

                     if R.Kind = VSS.JSON.Out_Of_Range
                       or else To_Unsigned_64 (R.Float_Value) /= Binary
                     then
                        Ada.Text_IO.Put ("FAIL: ");
                        Ada.Text_IO.Put (Buffer (First .. Last));
                        Ada.Text_IO.Put (' ');
                        Ada.Text_IO.Put (B1);
                        Ada.Text_IO.Put (' ');
                        Ada.Text_IO.Put (B2);
                        Ada.Text_IO.New_Line;
                     end if;

                     Test_Support.Assert
                       (To_Unsigned_64 (R.Float_Value) = Binary,
                        B2 & " (expected " & B1 & ')');
                  end if;
               end;
            end if;
         end if;
      end loop;

      Ada.Text_IO.Close (File);
   end Run_Test;

   --  This code is commented out intentionally, it is useful for debug of
   --  particular test case.
   --
   --  P : VSS.JSON.Implementation.Numbers.Parsing_State;
   --  N : VSS.JSON.JSON_Number;
   --  S : constant String := "358416272e-33";
   --  F : constant Interfaces.IEEE_Float_64 := 358416272.0e-33;

begin
   --  Process_String (P, S);
   --  P.Has_Fractional := True;
   --  VSS.JSON.Implementation.Numbers.To_JSON_Number
   --    (P, VSS.Strings.Conversions.To_Virtual_String (S), N);
   --  Test_Support.Assert (N.Float_Value = F);

   Load_Exceptions (Ada.Command_Line.Argument (1));

   for J in 2 .. Ada.Command_Line.Argument_Count loop
      Run_Test (Ada.Command_Line.Argument (J));
   end loop;

   Ada.Text_IO.Put_Line (Integer'Image (T));

exception
   when others =>
      Ada.Text_IO.Put_Line (Integer'Image (T));

      raise;
end Test_JSON_Decimal_To_Number;
