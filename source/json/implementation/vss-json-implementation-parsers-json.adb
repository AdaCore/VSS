--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  RFC 8259 "The JavaScript Object Notation (JSON) Data Interchange Format"

package body VSS.JSON.Implementation.Parsers.JSON is

   use VSS.Implementation.Character_Codes;
   use type VSS.JSON.Streams.JSON_Stream_Element_Kind;
   use type VSS.JSON.Pull_Readers.JSON_Reader_Error;
   use type VSS.Unicode.Code_Point_Unit;

   function Parse_JSON_Text
     (Parser : in out JSON_Parser_Base'Class) return Boolean;
   --  Parse 'json-text'.

   function Parse_Value
     (Self : in out JSON_Parser_Base'Class) return Boolean;
   --  Parse 'value'. Skip all leading whitespaces.

   function Parse_Array
     (Parser : in out JSON_Parser_Base'Class) return Boolean;

   function Parse_Object
     (Self : in out JSON_Parser_Base'Class) return Boolean;

   function Parse_Number
     (Parser : in out JSON_Parser_Base'Class) return Boolean;
   --  Parse number. When parse of number is done Number_Value event is
   --  reported, thus, subprogram returns False always.

   function Parse_String
     (Parser : in out JSON_Parser_Base'Class) return Boolean;

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
            if not Self.Stack.Is_Empty then
               raise Program_Error;
            end if;
         end if;
      end if;
   end Parse;

   -----------------
   -- Parse_Array --
   -----------------

   type Array_State is
     (Value_Or_End_Array,
      Value,
      Value_Separator_Or_End_Array,
      Finish);

   function Parse_Array
     (Parser : in out JSON_Parser_Base'Class) return Boolean
   is
      --  [RFC 8259]
      --
      --  array = begin-array [ value *( value-separator value ) ] end-array

      State : Array_State;
      Self  : JSON_Parser'Class renames JSON_Parser'Class (Parser);

   begin
      if not Self.Stack.Is_Empty then
         State := Array_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               return Self.Push (Parse_Array'Access, Array_State'Pos (State));
            end if;
         end if;

      else
         pragma Assert (Self.C = Begin_Array);

         State := Value_Or_End_Array;
         Self.Event := VSS.JSON.Streams.Start_Array;

         return Self.Push (Parse_Array'Access, Array_State'Pos (State));
      end if;

      loop
         case State is
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
                     Self.Event := VSS.JSON.Streams.End_Array;

                     return
                       Self.Push (Parse_Array'Access, Array_State'Pos (State));

                  when others =>
                     return
                       Self.Report_Error
                         ("value separator or end array expected");
               end case;

            when Value =>
               null;

            when Finish =>
               null;
         end case;

         if not Self.Read (Parse_Array'Access, Array_State'Pos (State)) then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State = Finish then
               return True;

            else
               return Self.Report_Error ("unexpected end of document");
            end if;
         end if;

         case State is
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

                     if not Parse_Value (Self) then
                        return
                          Self.Push
                            (Parse_Array'Access, Array_State'Pos (State));
                     end if;

                     raise Program_Error;

                  when End_Array =>
                     State := Finish;
                     Self.Event := VSS.JSON.Streams.End_Array;

                     return
                       Self.Push (Parse_Array'Access, Array_State'Pos (State));

                  when others =>
                     return Self.Report_Error ("value or end array expected");
               end case;

            when Value =>
               State := Value_Separator_Or_End_Array;

               if not Parse_Value (Self) then
                  return
                    Self.Push (Parse_Array'Access, Array_State'Pos (State));
               end if;

               raise Program_Error;

            when Value_Separator_Or_End_Array =>
               null;
               --  raise Program_Error;

            when Finish =>
               return True;
         end case;
      end loop;
   end Parse_Array;

   ---------------------
   -- Parse_JSON_Text --
   ---------------------

   type JSON_Text_State is (Initial, Whitespace_Or_End, Done);

   function Parse_JSON_Text
     (Parser : in out JSON_Parser_Base'Class) return Boolean
   is
      --  [RFC 8259]
      --
      --  JSON-text = ws value ws

      State : JSON_Text_State;
      Self  : JSON_Parser'Class renames JSON_Parser'Class (Parser);

   begin
      if not Self.Stack.Is_Empty then
         State := JSON_Text_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               if Self.Event /= VSS.JSON.Streams.Invalid
                 or else Self.Error /= VSS.JSON.Pull_Readers.Not_Valid
               then
                  Self.Stack.Push
                    (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

               else
                  State := Done;
                  Self.Stack.Push
                    (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));
               end if;

               return False;
            end if;
         end if;

      else
         State := Initial;
         Self.Event := VSS.JSON.Streams.Start_Document;

         return
           Self.Push (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));
      end if;

      loop
         case State is
            when Initial =>
               null;

            when Whitespace_Or_End =>
               case Self.C is
                  when Space
                     | Character_Tabulation
                     | Line_Feed
                     | Carriage_Return
                  =>
                     null;

                  when End_Of_Stream =>
                     null;

                  when others =>
                     State := Done;
                     Self.Stack.Push
                       (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

                     return Self.Report_Error ("end of document expected");
               end case;

            when Done =>
               Self.Event := VSS.JSON.Streams.End_Document;

               return True;
         end case;

         if not Self.Read
           (Parse_JSON_Text'Access, JSON_Text_State'Pos (State))
         then
            if not Self.Stream.Is_End_Of_Stream then
               return False;
            end if;

            State := Done;
         end if;

         case State is
            when Initial =>
               if not Parse_Value (Self) then
                  if Self.Event /= VSS.JSON.Streams.Invalid
                    or else Self.Error /= VSS.JSON.Pull_Readers.Not_Valid
                  then
                     State := Whitespace_Or_End;

                  else
                     State := Done;
                  end if;

                  Self.Stack.Push
                    (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

                  return False;

               else
                  raise Program_Error;
               end if;

            when Whitespace_Or_End =>
               --  Analysis will be done at the beginning of the next
               --  iteration

               null;

            when Done =>
               null;
         end case;
      end loop;
   end Parse_JSON_Text;

   ------------------
   -- Parse_Number --
   ------------------

   type Number_State is
     (Int,
      Int_Digits,
      Frac_Or_Exp,
      Frac_Digit,
      Frac_Digits,
      Exp_Sign_Or_Digits,
      Exp_Digit,
      Exp_Digits,
      Report_Value);

   function Parse_Number
     (Parser : in out JSON_Parser_Base'Class) return Boolean
   is
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

      Self  : JSON_Parser'Class renames JSON_Parser'Class (Parser);
      State : Number_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Number_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            raise Program_Error;
         end if;

      else
         Self.Buffer.Clear;
         VSS.JSON.Implementation.Numbers.Reset (Self.Number_State);

         case Self.C is
            when Hyphen_Minus =>
               State := Int;
               Self.Store_Character;
               Self.Number_State.Minus := True;

            when Digit_Zero =>
               State := Frac_Or_Exp;
               Self.Store_Character;

            when Digit_One .. Digit_Nine =>
               State := Int_Digits;
               Self.Store_Character;
               VSS.JSON.Implementation.Numbers.Int_Digit
                 (Self.Number_State, Self.C);

            when others =>
               raise Program_Error;
         end case;
      end if;

      loop
         case State is
            when Report_Value =>
               VSS.JSON.Implementation.Numbers.To_JSON_Number
                 (Self.Number_State,
                  Self.String_Value,
                  Self.Number);
               Self.Event := VSS.JSON.Streams.Number_Value;

               return False;

            when others =>
               null;
         end case;

         if not Self.Read (Parse_Number'Access, Number_State'Pos (State)) then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State
                    in Int_Digits | Frac_Or_Exp | Frac_Digits | Exp_Digits
                  --  XXX allowed states and conditions need to be checked.
            then
               State := Report_Value;

            else
               --  XXX Self.Stack.Push???

               raise Program_Error;
            end if;
         end if;

         case State is
            when Int =>
               case Self.C is
                  when Digit_Zero =>
                     State := Frac_Or_Exp;
                     Self.Store_Character;

                  when Digit_One .. Digit_Nine =>
                     State := Int_Digits;
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Int_Digit
                       (Self.Number_State, Self.C);

                  when others =>
                     return Self.Report_Error ("digit expected");
               end case;

            when Int_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Int_Digit
                       (Self.Number_State, Self.C);

                  when Decimal_Point =>
                     State := Frac_Digit;
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Decimal_Point
                       (Self.Number_State);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Store_Character;

                  when others =>
                     State := Report_Value;
               end case;

            when Frac_Or_Exp =>
               case Self.C is
                  when Decimal_Point =>
                     State := Frac_Digit;
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Decimal_Point
                       (Self.Number_State);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Store_Character;

                  when others =>
                     State := Report_Value;
               end case;

            when Frac_Digit =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Frac_Digits;
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Frac_Digit
                       (Self.Number_State, Self.C);

                  when others =>
                     return Self.Report_Error ("frac digit expected");
               end case;

            when Frac_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Frac_Digit
                       (Self.Number_State, Self.C);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Store_Character;

                  when others =>
                     State := Report_Value;
               end case;

            when Exp_Sign_Or_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Exp_Digits;
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Self.C);

                  when Hyphen_Minus =>
                     State := Exp_Digit;
                     Self.Store_Character;
                     Self.Number_State.Exp_Minus := True;

                  when Plus_Sign =>
                     State := Exp_Digit;
                     Self.Store_Character;

                  when others =>
                     return Self.Report_Error ("plus/minus or digit expected");
               end case;

            when Exp_Digit =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Exp_Digits;
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Self.C);

                  when others =>
                     return Self.Report_Error ("exp digit expected");
               end case;

            when Exp_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Store_Character;
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Self.C);

                  when others =>
                     State := Report_Value;
               end case;

            when Report_Value =>
               null;
         end case;
      end loop;
   end Parse_Number;

   ------------------
   -- Parse_Object --
   ------------------

   type Object_State is
     (Whitespace_Or_Member,
      Member_Or_End_Object,
      Member_String,
      Member_Name_Separator,
      Member_Value,
      Value_Separator_Or_End_Object,
      Finish);

   function Parse_Object
     (Self : in out JSON_Parser_Base'Class) return Boolean
   is
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
               return
                 Self.Push (Parse_Object'Access, Object_State'Pos (State));
            end if;
         end if;

      else
         pragma Assert (Self.C = Begin_Object);

         State := Member_Or_End_Object;
         Self.Event := VSS.JSON.Streams.Start_Object;

         return Self.Push (Parse_Object'Access, Object_State'Pos (State));
      end if;

      loop
         case State is
            when Member_Or_End_Object =>
               null;

            when Whitespace_Or_Member =>
               null;

            when Member_String =>
               State := Member_Name_Separator;
               Self.Event := VSS.JSON.Streams.Key_Name;

               return
                 Self.Push (Parse_Object'Access, Object_State'Pos (State));

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
                     return Self.Report_Error ("name separator expected");
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
                     Self.Event := VSS.JSON.Streams.End_Object;

                     return
                       Self.Push
                         (Parse_Object'Access, Object_State'Pos (State));

                  when others =>
                     return
                       Self.Report_Error
                         ("value separator or end object expected");
               end case;

            when Finish =>
               null;

            when Member_Value =>
               null;
         end case;

         if not Self.Read (Parse_Object'Access, Object_State'Pos (State)) then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State = Finish then
               return True;

            else
               return Self.Report_Error ("unexpected end of document");
            end if;
         end if;

         case State is
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

                     if not Parse_String (Self) then
                        return
                          Self.Push
                            (Parse_Object'Access, Object_State'Pos (State));
                     end if;

                  when End_Object =>
                     State := Finish;
                     Self.Event := VSS.JSON.Streams.End_Object;

                     return
                       Self.Push
                         (Parse_Object'Access, Object_State'Pos (State));

                  when others =>
                     return
                       Self.Report_Error ("string or end object expected");
               end case;

            when Member_String =>
               raise Program_Error;

            when Member_Name_Separator =>
               null;

            when Member_Value =>
               if not Parse_Value (Self) then
                  State := Value_Separator_Or_End_Object;

                  return
                    Self.Push (Parse_Object'Access, Object_State'Pos (State));

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

                     if not Parse_String (Self) then
                        return
                          Self.Push
                            (Parse_Object'Access, Object_State'Pos (State));
                     end if;

                  when others =>
                     return Self.Report_Error ("string expected");
               end case;

            when Finish =>
               return True;
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

   function Parse_String
     (Parser : in out JSON_Parser_Base'Class) return Boolean
   is

      use type VSS.Unicode.UTF16_Code_Unit;

      function Hex_To_Code
        (Code : in out VSS.Unicode.UTF16_Code_Unit) return Boolean;

      -----------------
      -- Hex_To_Code --
      -----------------

      function Hex_To_Code
        (Code : in out VSS.Unicode.UTF16_Code_Unit) return Boolean
      is
         Self  : JSON_Parser'Class renames JSON_Parser'Class (Parser);

      begin
         case Self.C is
            when Digit_Zero .. Digit_Nine =>
               Code :=
                 Code * 16#10#
                   + VSS.Unicode.UTF16_Code_Unit (Self.C - Digit_Zero);

               return True;

            when Latin_Capital_Letter_A .. Latin_Capital_Letter_F =>
               Code :=
                 Code * 16#10#
                   + VSS.Unicode.UTF16_Code_Unit
                       (Self.C - Latin_Capital_Letter_A + 10);

               return True;

            when Latin_Small_Letter_A .. Latin_Small_Letter_F =>
               Code :=
                 Code * 16#10#
                   + VSS.Unicode.UTF16_Code_Unit
                       (Self.C - Latin_Small_Letter_A + 10);

               return True;

            when others =>
               return False;
         end case;
      end Hex_To_Code;

      State : String_State;
      Self  : JSON_Parser'Class renames JSON_Parser'Class (Parser);

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
         Self.Buffer.Clear;
      end if;

      loop
         if not Self.Read (Parse_String'Access, String_State'Pos (State)) then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State = Finish then
               return True;

            else
               return Self.Report_Error ("premature end of string");
            end if;
         end if;

         case State is
            when Character_Data =>
               case Self.C is
                  when Quotation_Mark =>
                     State := Finish;

                  when 16#00_0000# .. 16#00_001F# =>
                     return Self.Report_Error ("unescaped control character");

                  when Reverse_Solidus =>
                     State := Escape;

                  when others =>
                     Self.Store_Character;
               end case;

            when Escape =>
               case Self.C is
                  when Quotation_Mark =>
                     State := Character_Data;
                     Self.Store_Character (Quotation_Mark);

                  when Reverse_Solidus =>
                     State := Character_Data;
                     Self.Store_Character (Reverse_Solidus);

                  when Solidus =>
                     State := Character_Data;
                     Self.Store_Character (Solidus);

                  when Latin_Small_Letter_B =>
                     State := Character_Data;
                     Self.Store_Character (Backspace);

                  when Latin_Small_Letter_F =>
                     State := Character_Data;
                     Self.Store_Character (Form_Feed);

                  when Latin_Small_Letter_N =>
                     State := Character_Data;
                     Self.Store_Character (Line_Feed);

                  when Latin_Small_Letter_R =>
                     State := Character_Data;
                     Self.Store_Character (Carriage_Return);

                  when Latin_Small_Letter_T =>
                     State := Character_Data;
                     Self.Store_Character (Character_Tabulation);

                  when Latin_Small_Letter_U =>
                     State := Escape_U;

                  when others =>
                     return
                       Self.Report_Error
                         ("invalid character in escape sequence");
               end case;

            when Escape_U =>
               State := Escape_UX;
               Self.Code_Unit_1 := 0;

               if not Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UX =>
               State := Escape_UXX;

               if not Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXX =>
               State := Escape_UXXX;

               if not Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXX =>
               if not Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

               if Self.Code_Unit_1 not in 16#D800# .. 16#DFFF# then
                  State := Character_Data;
                  Self.Store_Character (Self.Code_Unit_1);

               elsif Self.Code_Unit_1 in 16#D800# .. 16#DBFF# then
                  State := Escape_UXXXX;

               else
                  return
                    Self.Report_Error ("high surrogate code point unexpected");
               end if;

            when Escape_UXXXX =>
               case Self.C is
                  when Reverse_Solidus =>
                     State := Escape_UXXXX_Escape;

                  when others =>
                     return
                       Self.Report_Error
                         ("escaped low surrogate code point expected");
               end case;

            when Escape_UXXXX_Escape =>
               case Self.C is
                  when Latin_Small_Letter_U =>
                     State := Escape_UXXXX_Escape_U;

                  when others =>
                     return
                       Self.Report_Error
                         ("escaped low surrogate code point expected");
               end case;

            when Escape_UXXXX_Escape_U =>
               State := Escape_UXXXX_Escape_UX;
               Self.Code_Unit_2 := 0;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXXX_Escape_UX =>
               State := Escape_UXXXX_Escape_UXX;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXXX_Escape_UXX =>
               State := Escape_UXXXX_Escape_UXXX;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXXX_Escape_UXXX =>
               State := Character_Data;

               if not Hex_To_Code (Self.Code_Unit_2) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

               if Self.Code_Unit_2 not in 16#DC00# .. 16#DFFF# then
                  return
                    Self.Report_Error ("low surrogate code point expected");
               end if;

               Self.Store_Character (Self.Code_Unit_1, Self.Code_Unit_2);

            when Finish =>
               return True;
         end case;
      end loop;
   end Parse_String;

   -----------------
   -- Parse_Value --
   -----------------

   type Value_State is
     (Initial,
      Value_String,
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
      Finish);

   function Parse_Value
     (Self : in out JSON_Parser_Base'Class) return Boolean
   is
      State   : Value_State;
      Success : Boolean;

   begin
      if not Self.Stack.Is_Empty then
         State := Value_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               return Self.Push (Parse_Value'Access, Value_State'Pos (State));
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
                     if not Parse_String (Self) then
                        State := Value_String;

                        return
                          Self.Push
                            (Parse_Value'Access, Value_State'Pos (State));

                     else
                        Self.Event := VSS.JSON.Streams.String_Value;

                        return False;
                     end if;

                  when Latin_Small_Letter_F =>
                     State := Value_F;

                  when Latin_Small_Letter_N =>
                     State := Value_N;

                  when Latin_Small_Letter_T =>
                     State := Value_T;

                  when Hyphen_Minus | Digit_Zero .. Digit_Nine =>
                     Success := Parse_Number (Self);
                     pragma Assert (not Success);  --  Always return False

                     return False;

                  when Begin_Array =>
                     if not Parse_Array (Self) then
                        return False;

                     else
                        --  Parse_Array always returns False for the first
                        --  call: it reports Start_Array event.

                        raise Program_Error;
                     end if;

                  when Begin_Object =>
                     if not Parse_Object (Self) then
                        return False;

                     else
                        --  Parse_Object always returns False for the first
                        --  call: it reports Start_Array event.

                        raise Program_Error;
                     end if;

                  when others =>
                     return Self.Report_Error ("value expected");
               end case;

            when Value_String =>
               Self.Event := VSS.JSON.Streams.String_Value;

               return False;

            when Value_F | Value_FA | Value_FAL | Value_FALS =>
               null;

            when Value_N | Value_NU | Value_NUL =>
               null;

            when Value_T | Value_TR | Value_TRU =>
               null;

            when Finish =>
               null;
         end case;

         if not Self.Read (Parse_Value'Access, Value_State'Pos (State)) then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State = Finish then
               return True;

            else
               return Self.Report_Error ("premature end of value");
            end if;
         end if;

         case State is
            when Initial =>
               null;

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
                     Self.Event := VSS.JSON.Streams.Boolean_Value;

                     return
                       Self.Push (Parse_Value'Access, Value_State'Pos (State));

                  when others =>
                     return Self.Report_Error ("false expected");
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
                     Self.Event := VSS.JSON.Streams.Null_Value;

                     return
                       Self.Push (Parse_Value'Access, Value_State'Pos (State));

                  when others =>
                     return Self.Report_Error ("null expected");
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
                     Self.Event := VSS.JSON.Streams.Boolean_Value;

                     return
                       Self.Push (Parse_Value'Access, Value_State'Pos (State));

                  when others =>
                     return Self.Report_Error ("true expected");
               end case;

            when Finish =>
               return True;
         end case;
      end loop;
   end Parse_Value;

end VSS.JSON.Implementation.Parsers.JSON;
