------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--  RFC 8259 "The JavaScript Object Notation (JSON) Data Interchange Format"

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Magic.Characters;
with Magic.Strings.Conversions;

package body Magic.JSON.Implementation.Parsers is

   --  procedure Parse_JSON_Text
   --    (Self   : in out JSON_Parser'Class;
   --     Status : in out Parse_Status);

   function Parse_JSON_Text
     (Self : in out JSON_Parser'Class) return Boolean;
   --  Parse 'json-text'.

   function Parse_Value (Self : in out JSON_Parser'Class) return Boolean;
   --  Parse 'value'. Skip all leading whitespaces.

   function Parse_Array (Self : in out JSON_Parser'Class) return Boolean;

   function Parse_Object (Self : in out JSON_Parser'Class) return Boolean;

   function Parse_Number (Self : in out JSON_Parser'Class) return Boolean;

   function Parse_String (Self : in out JSON_Parser'Class) return Boolean;

   function Read
     (Self  : in out JSON_Parser'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) return Boolean;
   --  Attempt to read next character from the text stream. Return True is
   --  operation is successful; otherwise push (Parse, State) pair into the
   --  parser's state stack and return False.

   function Report_Error
     (Self    : in out JSON_Parser'Class;
      Message : Wide_Wide_String) return Boolean;
   --  Set parser into document not valid state. Always return False.

   Backspace              : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_0008#);
   Character_Tabulation   : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_0009#);
   Line_Feed              : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_000A#);
   Form_Feed              : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_000C#);
   Carriage_Return        : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_000D#);
   Space                  : constant Wide_Wide_Character := ' ';  --  U+0020
   Quotation_Mark         : constant Wide_Wide_Character := '"';  --  U+0022
   Hyphen_Minus           : constant Wide_Wide_Character := '-';
   Plus_Sign              : constant Wide_Wide_Character := '+';
   Reverse_Solidus        : constant Wide_Wide_Character := '\';  --  U+005C
   Solidus                : constant Wide_Wide_Character := '/';  --  U+002F
   Digit_Zero             : constant Wide_Wide_Character := '0';
   Digit_One              : constant Wide_Wide_Character := '1';
   Digit_Nine             : constant Wide_Wide_Character := '9';
   Latin_Capital_Letter_A : constant Wide_Wide_Character := 'A';  --  U+0041
   Latin_Capital_Letter_E : constant Wide_Wide_Character := 'E';  --  U+0045
   Latin_Capital_Letter_F : constant Wide_Wide_Character := 'F';  --  U+0046
   Latin_Small_Letter_A   : constant Wide_Wide_Character := 'a';
   Latin_Small_Letter_B   : constant Wide_Wide_Character := 'b';  --  U+0062
   Latin_Small_Letter_E   : constant Wide_Wide_Character := 'e';
   Latin_Small_Letter_F   : constant Wide_Wide_Character := 'f';  --  U+0066
   Latin_Small_Letter_L   : constant Wide_Wide_Character := 'l';
   Latin_Small_Letter_N   : constant Wide_Wide_Character := 'n';  --  U+006E
   Latin_Small_Letter_R   : constant Wide_Wide_Character := 'r';  --  U+0072
   Latin_Small_Letter_S   : constant Wide_Wide_Character := 's';  --  U+0071
   Latin_Small_Letter_T   : constant Wide_Wide_Character := 't';  --  U+0074
   Latin_Small_Letter_U   : constant Wide_Wide_Character := 'u';  --  U+0075

   Begin_Array            : constant Wide_Wide_Character := '[';
   Begin_Object           : constant Wide_Wide_Character := '{';
   End_Array              : constant Wide_Wide_Character := ']';
   End_Object             : constant Wide_Wide_Character := '}';
   Name_Separator         : constant Wide_Wide_Character := ':';
   Value_Separator        : constant Wide_Wide_Character := ',';
   Decimal_Point          : constant Wide_Wide_Character := '.';

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value (Self : JSON_Parser'Class) return Boolean is
   begin
      return Self.Boolean;
   end Boolean_Value;

   -----------
   -- Error --
   -----------

   function Error
     (Self : JSON_Parser'Class)
      return Magic.JSON.Streams.Readers.JSON_Reader_Error is
   begin
      return Self.Error;
   end Error;

   ----------------
   -- Event_Kind --
   ----------------

   function Event_Kind
     (Self : JSON_Parser'Class)
      return Magic.JSON.Streams.Readers.JSON_Event_Kind is
   begin
      return Self.Event;
   end Event_Kind;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Parse_Stack'Class) return Boolean is
   begin
      return Self.Head = 0;
   end Is_Empty;

   ------------------
   -- Number_Value --
   ------------------

   function Number_Value
     (Self : JSON_Parser'Class) return Magic.JSON.JSON_Number is
   begin
      return Self.Number;
   end Number_Value;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self : in out JSON_Parser'Class) is
   begin
      if Self.Stack.Is_Empty then
         if Parse_JSON_Text (Self) then
            raise Program_Error;
         end if;

      else
         if Self.Stack.Top.Parse (Self) then
            raise Program_Error;
         end if;
      end if;
   end Parse;

   -----------------
   -- Parse_Array --
   -----------------

   type Array_State is
     (Initial,
      Value_Or_End_Array,
      Value,
      Value_Separator_Or_End_Array,
      Finish);

   function Parse_Array (Self : in out JSON_Parser'Class) return Boolean is
      --  [RFC 8259]
      --
      --  array = begin-array [ value *( value-separator value ) ] end-array

      State : Array_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Array_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               Self.Stack.Push
                 (Parse_Array'Access, Array_State'Pos (State));

               return False;
            end if;
         end if;

      else
         State := Initial;
      end if;

      loop
         case State is
            when Initial =>
               if Self.C /= Begin_Array then
                  raise Program_Error;
               end if;

               State := Value_Or_End_Array;
               Self.Nesting := Self.Nesting + 1;
               Self.Event := Magic.JSON.Streams.Readers.Start_Array;
               Self.Stack.Push (Parse_Array'Access, Array_State'Pos (State));

               return False;

            when Value_Or_End_Array =>
               null;

            when Value_Separator_Or_End_Array =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when Value_Separator =>
                     State := Value;

                  when End_Array =>
                     State := Finish;
                     Self.Nesting := Self.Nesting - 1;
                     Self.Event := Magic.JSON.Streams.Readers.End_Array;
                     Self.Stack.Push
                       (Parse_Array'Access, Array_State'Pos (State));

                     return False;

                  when others =>
                     return
                       Self.Report_Error
                         ("value separator of end array expected");
               end case;

            when Value =>
               null;

            when Finish =>
               null;

            when others =>
               raise Program_Error;
         end case;

         if not Self.Read (Parse_Array'Access, Array_State'Pos (State)) then
            --  XXX Need to be reviewed!!!

            if Self.Stream.Is_End_Of_Stream and Self.Nesting = 0 then
               Self.Stack.Pop;

               return True;

            else
               return False;
            end if;
         end if;

         case State is
            when Initial =>
               raise Program_Error;
               --  Initial state is used to report Start_Array event.

            when Value_Or_End_Array =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when Begin_Array
                     | Begin_Object
                     | Quotation_Mark
                     | Hyphen_Minus
                     | Digit_Zero .. Digit_Nine
                     | Latin_Small_Letter_F
                     | Latin_Small_Letter_N
                     | Latin_Small_Letter_T
                  =>
                     State := Value_Separator_Or_End_Array;

                     if not Self.Parse_Value then
                        Self.Stack.Push
                          (Parse_Array'Access, Array_State'Pos (State));

                        return False;
                     end if;

                     raise Program_Error;

                  when End_Array =>
                     State := Finish;
                     Self.Nesting := Self.Nesting - 1;
                     Self.Event := Magic.JSON.Streams.Readers.End_Array;
                     Self.Stack.Push
                       (Parse_Array'Access, Array_State'Pos (State));

                     return False;

                  when others =>
                     return Self.Report_Error ("value of end array expected");
               end case;

            when Value =>
               State := Value_Separator_Or_End_Array;

               if not Self.Parse_Value then
                  Self.Stack.Push
                    (Parse_Array'Access, Array_State'Pos (State));

                  return False;
               end if;

               raise Program_Error;

            when Value_Separator_Or_End_Array =>
               null;
               --  raise Program_Error;

            when Finish =>
               return True;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Parse_Array;

   ---------------------
   -- Parse_JSON_Text --
   ---------------------

   type JSON_Text_State is (Initial, Value, End_Of_Stream);

   function Parse_JSON_Text
     (Self : in out JSON_Parser'Class) return Boolean
   is
      --  [RFC 8259]
      --
      --  JSON-text = ws value ws

      State : JSON_Text_State;

   begin
      if not Self.Stack.Is_Empty then
         State := JSON_Text_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               Self.Stack.Push
                 (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

               return False;
            end if;
         end if;

      else
         State := Initial;
         Self.Event := Magic.JSON.Streams.Readers.Start_Document;
         Self.Stack.Push (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

         return False;
      end if;

      loop
         case State is
            when Initial =>
               null;

            when Value =>
               null;

            when End_Of_Stream =>
               Self.Event := Magic.JSON.Streams.Readers.No_Token;
               Self.Stack.Push
                 (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

               return False;

            when others =>
               raise Program_Error;
         end case;

         if not Self.Read
           (Parse_JSON_Text'Access, JSON_Text_State'Pos (State))
         then
            if Self.Stream.Is_End_Of_Stream then
               Self.Stack.Pop;

               State := End_Of_Stream;
               Self.Event := Magic.JSON.Streams.Readers.End_Document;
               Self.Stack.Push
                 (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));
            end if;

            return False;
         end if;

         case State is
            when Initial =>
               if not Self.Parse_Value then
                  State := Value;
                  Self.Stack.Push
                    (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

                  return False;

               else
                  raise Program_Error;
               end if;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Parse_JSON_Text;

   ------------------
   -- Parse_Number --
   ------------------

   type Number_State is
     (Int, Int_Digits, Frac_Or_Exp, Frac_Digits, Exp_Sign_Or_Digits, Exp_Digits);

   function Parse_Number (Self : in out JSON_Parser'Class) return Boolean is
      --  [RFC 8259]
      --
      --  number = [ minus ] int [ frac ] [ exp ]
      --
      --  decimal-point = %x2E       ; .
      --
      --  digit1-9 = %x31-39         ; 1-9
      --
      --  e = %x65 / %x45            ; e E
      --
      --  exp = e [ minus / plus ] 1*DIGIT
      --
      --  frac = decimal-point 1*DIGIT
      --
      --  int = zero / ( digit1-9 *DIGIT )
      --
      --  minus = %x2D               ; -
      --
      --  plus = %x2B                ; +
      --
      --  zero = %x30                ; 0

      procedure Convert_Number;

      --------------------
      -- Convert_Number --
      --------------------

      procedure Convert_Number is
         Image : constant Wide_Wide_String :=
           Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Self.String);

      begin
         if not Self.Is_Float then
            Self.Number :=
              (Kind          => Magic.JSON.JSON_Integer,
               Integer_Value => Interfaces.Integer_64'Wide_Wide_Value (Image));

         else
            Self.Number :=
              (Kind        => Magic.JSON.JSON_Float,
               Float_Value =>
                 Interfaces.IEEE_Float_64'Wide_Wide_Value (Image));
         end if;
      end Convert_Number;

      State : Number_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Number_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            raise Program_Error;
         end if;

      else
         Self.String :=
           Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
         Self.Is_Float := False;

         case Self.C is
            when Hyphen_Minus =>
               State := Int;
               Ada.Strings.Wide_Wide_Unbounded.Append (Self.String, Self.C);

            when Digit_Zero =>
               State := Frac_Or_Exp;
               Ada.Strings.Wide_Wide_Unbounded.Append (Self.String, Self.C);

            when Digit_One .. Digit_Nine =>
               State := Int_Digits;
               Ada.Strings.Wide_Wide_Unbounded.Append (Self.String, Self.C);

            when others =>
               raise Program_Error;
         end case;
      end if;

      loop
         if not Self.Read (Parse_Number'Access, Number_State'Pos (State)) then
            if Self.Stream.Is_End_Of_Stream
              and Self.Nesting = 0
              and State in Int_Digits | Frac_Or_Exp | Frac_Digits | Exp_Digits
              --  XXX allowed states and conditions need to be checked.
            then
               --  Simulate successful read when 'string' parsing has been
               --  finished, 'string' is not nested into another construct,
               --  and end of stream has been reached.

               Self.Stack.Pop;
               Self.C := Wide_Wide_Character'Last;

               Convert_Number;

               return True;

            else
               return False;
            end if;
         end if;

         case State is
            when Int =>
               case Self.C is
                  when Digit_Zero =>
                     State := Frac_Or_Exp;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when Digit_One .. Digit_Nine =>
                     State := Int_Digits;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when others =>
                     raise Program_Error;
               end case;

            when Int_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when Decimal_Point =>
                     State := Frac_Digits;
                     Self.Is_Float := True;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Is_Float := True;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when others =>
                     Convert_Number;

                     return True;
               end case;

            when Frac_Or_Exp =>
               case Self.C is
                  when Decimal_Point =>
                     State := Frac_Digits;
                     Self.Is_Float := True;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Is_Float := True;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when others =>
                     Convert_Number;

                     return True;
               end case;

            when Frac_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when others =>
                     Convert_Number;

                     return True;
               end case;

            when Exp_Sign_Or_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Exp_Digits;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when Hyphen_Minus =>
                     State := Exp_Digits;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when Plus_Sign =>
                     State := Exp_Digits;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when others =>
                     raise Program_Error;
               end case;

            when Exp_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Exp_Digits;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);

                  when others =>
                     Convert_Number;

                     return True;
               end case;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Parse_Number;

   ------------------
   -- Parse_Object --
   ------------------

   type Object_State is
     (Initial,
      Whitespace_Or_Member,
      Member_Or_End_Object,
      Member_String,
      Member_Name_Separator,
      Member_Value,
      Value_Separator_Or_End_Object,
      Finish);

   function Parse_Object (Self : in out JSON_Parser'Class) return Boolean is
      --  [RFC 8259]
      --
      --  object = begin-object [ member *( value-separator member ) ]
      --           end-object
      --
      --  member = string name-separator value

      State : Object_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Object_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               Self.Stack.Push (Parse_Object'Access, Object_State'Pos (State));

               return False;
            end if;
         end if;

      else
         State := Initial;
      end if;

      loop
         case State is
            when Initial =>
               if Self.C /= Begin_Object then
                  raise Program_Error;
               end if;

               State := Member_Or_End_Object;
               Self.Event := Magic.JSON.Streams.Readers.Start_Object;
               Self.Nesting := Self.Nesting + 1;
               Self.Stack.Push (Parse_Object'Access, Object_State'Pos (State));

               return False;

            when Member_Or_End_Object =>
               null;

            when Whitespace_Or_Member =>
               null;

            when Member_String =>
               State := Member_Name_Separator;
               Self.Event := Magic.JSON.Streams.Readers.Key_Name;
               Self.Stack.Push
                 (Parse_Object'Access, Object_State'Pos (State));

               return False;

            when Member_Name_Separator =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when Name_Separator =>
                     State := Member_Value;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_Separator_Or_End_Object =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when Value_Separator =>
                     State := Whitespace_Or_Member;

                  when End_Object =>
                     State := Finish;
                     Self.Event := Magic.JSON.Streams.Readers.End_Object;
                     Self.Nesting := Self.Nesting - 1;
                     Self.Stack.Push
                       (Parse_Object'Access, Object_State'Pos (State));

                     return False;

                  when others =>
                     raise Program_Error;
               end case;

            when Finish =>
               null;

            when others =>
               raise Program_Error;
         end case;

         if not Self.Read (Parse_Object'Access, Object_State'Pos (State)) then
            --  XXX Need to be reviewed!!!

            if Self.Stream.Is_End_Of_Stream and Self.Nesting = 0 then
               Self.Stack.Pop;

               return True;

            else
               return False;
            end if;
         end if;

         case State is
            when Initial =>
               raise Program_Error;

            when Member_Or_End_Object =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when Quotation_Mark =>
                     State := Member_String;

                     if not Self.Parse_String then
                        Self.Stack.Push
                          (Parse_Object'Access, Object_State'Pos (State));

                        return False;
                     end if;

                  when End_Object =>
                     State := Finish;
                     Self.Event := Magic.JSON.Streams.Readers.End_Object;
                     Self.Nesting := Self.Nesting - 1;
                     Self.Stack.Push
                       (Parse_Object'Access, Object_State'Pos (State));

                     return False;

                  when others =>
                     raise Program_Error;
               end case;

            when Member_String =>
               raise Program_Error;

            when Member_Name_Separator =>
               null;

            when Member_Value =>
               if not Self.Parse_Value then
                  State := Value_Separator_Or_End_Object;
                  Self.Stack.Push
                    (Parse_Object'Access, Object_State'Pos (State));

                  return False;

               else
                  raise Program_Error;
               end if;

            when Value_Separator_Or_End_Object =>
               null;

            when Whitespace_Or_Member =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when Quotation_Mark =>
                     State := Member_String;

                     if not Self.Parse_String then
                        Self.Stack.Push
                          (Parse_Object'Access, Object_State'Pos (State));

                        return False;
                     end if;

                  when others =>
                     raise Program_Error;
               end case;

            when Finish =>
               return True;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Parse_Object;

   ------------------
   -- Parse_String --
   ------------------

   type String_State is
     (Character_Data,
      Escape,
      Escape_U,
      Escape_UX,
      Escape_UXX,
      Escape_UXXX,
      Escape_UXXXX,
      Escape_UXXXX_Escape,
      Escape_UXXXX_Escape_U,
      Escape_UXXXX_Escape_UX,
      Escape_UXXXX_Escape_UXX,
      Escape_UXXXX_Escape_UXXX,
      Finish);

   function Parse_String (Self : in out JSON_Parser'Class) return Boolean is

      use type Magic.Unicode.Code_Point;
      use type Magic.Unicode.UTF16_Code_Unit;

      function Hex_To_Code
        (Code : in out Magic.Unicode.UTF16_Code_Unit) return Boolean;

      -----------------
      -- Hex_To_Code --
      -----------------

      function Hex_To_Code
        (Code : in out Magic.Unicode.UTF16_Code_Unit) return Boolean is
      begin
         case Self.C is
            when Digit_Zero .. Digit_Nine =>
               Code :=
                 Code * 16#10#
                   + (Wide_Wide_Character'Pos (Self.C)
                        - Wide_Wide_Character'Pos (Digit_Zero));

               return True;

            when Latin_Capital_Letter_A .. Latin_Capital_Letter_F =>
               Code :=
                 Code * 16#10#
                   + (Wide_Wide_Character'Pos (Self.C)
                        - Wide_Wide_Character'Pos (Latin_Capital_Letter_A) + 10);

               return True;

            when Latin_Small_Letter_A .. Latin_Small_Letter_F =>
               Code :=
                 Code * 16#10#
                   + (Wide_Wide_Character'Pos (Self.C)
                        - Wide_Wide_Character'Pos (Latin_Small_Letter_A) + 10);

               return True;

            when others =>
               return False;
         end case;
      end Hex_To_Code;

      State : String_State;

   begin
      if not Self.Stack.Is_Empty then
         State := String_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            raise Program_Error;
         end if;

      else
         if Self.C /= Quotation_Mark then
            raise Program_Error;
         end if;

         State := Character_Data;
         Self.String :=
           Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;
      end if;

      loop
         if not Self.Read (Parse_String'Access, String_State'Pos (State)) then
            if Self.Stream.Is_End_Of_Stream
              and Self.Nesting = 0
              and State = Finish
            then
               --  Simulate successful read when 'string' parsing has been
               --  finished, 'string' is not nested into another construct,
               --  and end of stream has been reached.

               Self.Stack.Pop;
               Self.C := Wide_Wide_Character'Last;

               return True;

            else
               return False;
            end if;
         end if;

         case State is
            when Character_Data =>
               case Self.C is
                  when Quotation_Mark =>
                     State := Finish;

                  when Wide_Wide_Character'Val (16#00_0000#)
                     .. Wide_Wide_Character'Val (16#00_001F#)
                  =>
                     raise Program_Error;

                  when Reverse_Solidus =>
                     State := Escape;

                  when others =>
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Self.C);
               end case;

            when Escape =>
               case Self.C is
                  when Quotation_Mark =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Quotation_Mark);

                  when Reverse_Solidus =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Reverse_Solidus);

                  when Solidus =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Solidus);

                  when Latin_Small_Letter_B =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Backspace);

                  when Latin_Small_Letter_F =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Form_Feed);

                  when Latin_Small_Letter_N =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Line_Feed);

                  when Latin_Small_Letter_R =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Carriage_Return);

                  when Latin_Small_Letter_T =>
                     State := Character_Data;
                     Ada.Strings.Wide_Wide_Unbounded.Append
                       (Self.String, Character_Tabulation);

                  when Latin_Small_Letter_U =>
                     State := Escape_U;

                  when others =>
                     raise Program_Error;
               end case;

            when Escape_U =>
               State := Escape_UX;
               Self.Code_Unit_1 := 0;

               if not Hex_To_Code (Self.Code_Unit_1) then
                  raise Program_Error;
               end if;

            when Escape_UX =>
               State := Escape_UXX;

               if not Hex_To_Code (Self.Code_Unit_1) then
                  raise Program_Error;
               end if;

            when Escape_UXX =>
               State := Escape_UXXX;

               if not Hex_To_Code (Self.Code_Unit_1) then
                  raise Program_Error;
               end if;

            when Escape_UXXX =>
               if not Hex_To_Code (Self.Code_Unit_1) then
                  raise Program_Error;
               end if;

               if Self.Code_Unit_1 not in 16#D800# .. 16#DFFF# then
                  State := Character_Data;
                  Ada.Strings.Wide_Wide_Unbounded.Append
                    (Self.String, Wide_Wide_Character'Val (Self.Code_Unit_1));

               elsif Self.Code_Unit_1 in 16#D800# .. 16#DBFF# then
                  State := Escape_UXXXX;

               else
                  raise Program_Error;
               end if;

            when Escape_UXXXX =>
               case Self.C is
                  when Reverse_Solidus =>
                     State := Escape_UXXXX_Escape;

                  when others =>
                     raise Program_Error;
               end case;

            when Escape_UXXXX_Escape =>
               case Self.C is
                  when Latin_Small_Letter_U =>
                     State := Escape_UXXXX_Escape_U;

                  when others =>
                     raise Program_Error;
               end case;

            when Escape_UXXXX_Escape_U =>
               State := Escape_UXXXX_Escape_UX;
               Self.Code_Unit_2 := 0;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  raise Program_Error;
               end if;

            when Escape_UXXXX_Escape_UX =>
               State := Escape_UXXXX_Escape_UXX;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  raise Program_Error;
               end if;

            when Escape_UXXXX_Escape_UXX =>
               State := Escape_UXXXX_Escape_UXXX;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  raise Program_Error;
               end if;

            when Escape_UXXXX_Escape_UXXX =>
               State := Character_Data;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  raise Program_Error;
               end if;

               if Self.Code_Unit_2 not in 16#DC00# .. 16#DFFF# then
                  raise Program_Error;
               end if;

               declare
                  Code : Magic.Unicode.Code_Point := 16#01_0000#;

               begin
                  Code :=
                    Code
                      + Magic.Unicode.Code_Point
                         (Self.Code_Unit_1 and 16#03FF#) * 16#0400#
                      + Magic.Unicode.Code_Point
                         (Self.Code_Unit_2 and 16#03FF#);
                  Ada.Strings.Wide_Wide_Unbounded.Append
                    (Self.String, Wide_Wide_Character'Val (Code));
               end;

            when Finish =>
               return True;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Parse_String;

   -----------------
   -- Parse_Value --
   -----------------

   type Value_State is
     (Initial,
      Value_String,
      Value_Number,
      Value_Array,
      Value_F,
      Value_FA,
      Value_FAL,
      Value_FALS,
      Value_N,
      Value_NU,
      Value_NUL,
      Value_T,
      Value_TR,
      Value_TRU,
      Finish,
      Done);

   function Parse_Value (Self : in out JSON_Parser'Class) return Boolean is
      State : Value_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Value_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               Self.Stack.Push (Parse_Value'Access, Value_State'Pos (State));

               return False;
            end if;
         end if;

      else
         State := Initial;
      end if;

      loop
         case State is
            when Initial =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when Quotation_Mark =>
                     if not Self.Parse_String then
                        State := Value_String;
                        Self.Stack.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;

                     else
                        State := Done;
                        Self.Event := Magic.JSON.Streams.Readers.String_Value;
                        Self.Stack.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;
                     end if;

                  when Latin_Small_Letter_F =>
                     State := Value_F;

                  when Latin_Small_Letter_N =>
                     State := Value_N;

                  when Latin_Small_Letter_T =>
                     State := Value_T;

                  when Hyphen_Minus | Digit_Zero .. Digit_Nine =>
                     if not Self.Parse_Number then
                        State := Value_Number;
                        Self.Stack.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;

                     else
                        State := Done;
                        Self.Event := Magic.JSON.Streams.Readers.Number_Value;
                        Self.Stack.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;
                     end if;

                  when Begin_Array =>
                     if not Self.Parse_Array then
                        State := Done;
                        Self.Stack.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;

                     else
                        --  Parse_Array always reeturns False for the first
                        --  call: it reports Start_Array event.

                        raise Program_Error;
                     end if;

                  when Begin_Object =>
                     if not Self.Parse_Object then
                        State := Done;
                        Self.Stack.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;

                     else
                        --  Parse_Object always reeturns False for the first
                        --  call: it reports Start_Array event.

                        raise Program_Error;
                     end if;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_String =>
               State := Done;
               Self.Event := Magic.JSON.Streams.Readers.String_Value;
               Self.Stack.Push
                 (Parse_Value'Access, Value_State'Pos (State));

               return False;

            when Value_Number =>
               State := Done;
               Self.Event := Magic.JSON.Streams.Readers.Number_Value;
               Self.Stack.Push
                 (Parse_Value'Access, Value_State'Pos (State));

               return False;

            when Value_F | Value_FA | Value_FAL | Value_FALS =>
               null;

            when Value_N | Value_NU | Value_NUL =>
               null;

            when Value_T | Value_TR | Value_TRU =>
               null;

            when Finish =>
               null;

            when Done =>
               return True;

            when others =>
               null;
         end case;

         if not Self.Read (Parse_Value'Access, Value_State'Pos (State)) then
            if Self.Stream.Is_End_Of_Stream
              and Self.Nesting = 0
              and State = Finish
            then
               --  Simulate successful read when 'string' parsing has been
               --  finished, 'string' is not nested into another construct,
               --  and end of stream has been reached.

               Self.Stack.Pop;
               Self.C := Wide_Wide_Character'Last;

               return True;

            else
               return False;
            end if;
         end if;

         case State is
            when Initial =>
               null;

            when Value_Array =>
               raise Program_Error;

            when Value_String =>
               raise Program_Error;

            when Value_F =>
               case Self.C is
                  when Latin_Small_Letter_A =>
                     State := Value_FA;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_FA =>
               case Self.C is
                  when Latin_Small_Letter_L =>
                     State := Value_FAL;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_FAL =>
               case Self.C is
                  when Latin_Small_Letter_S =>
                     State := Value_FALS;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_FALS =>
               case Self.C is
                  when Latin_Small_Letter_E =>
                     State := Finish;
                     Self.Boolean := False;
                     Self.Event := Magic.JSON.Streams.Readers.Boolean_Value;
                     Self.Stack.Push
                       (Parse_Value'Access, Value_State'Pos (State));

                     return False;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_N =>
               case Self.C is
                  when Latin_Small_Letter_U =>
                     State := Value_NU;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_NU =>
               case Self.C is
                  when Latin_Small_Letter_L =>
                     State := Value_NUL;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_NUL =>
               case Self.C is
                  when Latin_Small_Letter_L =>
                     State := Finish;
                     Self.Event := Magic.JSON.Streams.Readers.Null_Value;
                     Self.Stack.Push
                       (Parse_Value'Access, Value_State'Pos (State));

                     return False;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_T =>
               case Self.C is
                  when Latin_Small_Letter_R =>
                     State := Value_TR;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_TR =>
               case Self.C is
                  when Latin_Small_Letter_U =>
                     State := Value_TRU;

                  when others =>
                     raise Program_Error;
               end case;

            when Value_TRU =>
               case Self.C is
                  when Latin_Small_Letter_E =>
                     State := Finish;
                     Self.Boolean := True;
                     Self.Event := Magic.JSON.Streams.Readers.Boolean_Value;
                     Self.Stack.Push
                       (Parse_Value'Access, Value_State'Pos (State));

                     return False;

                  when others =>
                     raise Program_Error;
               end case;

            when Finish =>
               return True;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Parse_Value;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : in out Parse_Stack'Class) is
   begin
      Self.Head := Self.Head - 1;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self  : in out Parse_Stack'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) is
   begin
      Self.Head := Self.Head + 1;
      Self.Stack (Self.Head) := (Parse, State);
   end Push;

   ----------
   -- Read --
   ----------

   function Read
     (Self  : in out JSON_Parser'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) return Boolean
   is
      Success   : Boolean := True;
      Character : Magic.Characters.Magic_Character;

   begin
      Self.Stream.Get (Character, Success);

      if not Success then
         Self.Event := Magic.JSON.Streams.Readers.Invalid;
         Self.Error := Magic.JSON.Streams.Readers.Premature_End_Of_Document;
         Self.Stack.Push (Parse, State);

         return False;

      else
         Self.C := Wide_Wide_Character (Character);
      end if;

      Put_Line (''' & Self.C & ''');

      return True;
   end Read;

   ------------------
   -- Report_Error --
   ------------------

   function Report_Error
     (Self    : in out JSON_Parser'Class;
      Message : Wide_Wide_String) return Boolean is
   begin
      Self.Event := Magic.JSON.Streams.Readers.Invalid;
      Self.Error := Magic.JSON.Streams.Readers.Not_Valid;
      --  Self.Message := Magic.Strings.Conversions.To_Magic_String (Message);

      return False;
   end Report_Error;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Parser'Class;
      Stream : not null Magic.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Stream := Stream;
   end Set_Stream;

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (Self : JSON_Parser'Class) return Magic.Strings.Magic_String is
   begin
      return
        Magic.Strings.Conversions.To_Magic_String
          (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
             (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                (Self.String)));
   end String_Value;

   ---------
   -- Top --
   ---------

   function Top (Self : Parse_Stack'Class) return Parse_State is
   begin
      return Self.Stack (Self.Head);
   end Top;

end Magic.JSON.Implementation.Parsers;
