--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  RFC 8259 "The JavaScript Object Notation (JSON) Data Interchange Format"

with VSS.Characters;

package body VSS.JSON.Implementation.Parsers is

   use type VSS.JSON.Pull_Readers.JSON_Event_Kind;
   use type VSS.JSON.Pull_Readers.JSON_Reader_Error;

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

   ------------
   -- At_End --
   ------------

   function At_End (Self : JSON_Parser'Class) return Boolean is
   begin
      return
        Self.Stack.Is_Empty and
          (Self.Stream.Is_End_Of_Stream
           or Self.Error = VSS.JSON.Pull_Readers.Not_Valid);
   end At_End;

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
      return VSS.JSON.Pull_Readers.JSON_Reader_Error is
   begin
      return Self.Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : JSON_Parser'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Message;
   end Error_Message;

   ----------------
   -- Event_Kind --
   ----------------

   function Event_Kind
     (Self : JSON_Parser'Class) return VSS.JSON.Pull_Readers.JSON_Event_Kind is
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
     (Self : JSON_Parser'Class) return VSS.JSON.JSON_Number is
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
               Self.Push (Parse_Array'Access, Array_State'Pos (State));

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
               Self.Event := VSS.JSON.Pull_Readers.Start_Array;
               Self.Push (Parse_Array'Access, Array_State'Pos (State));

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
                     Self.Event := VSS.JSON.Pull_Readers.End_Array;
                     Self.Push (Parse_Array'Access, Array_State'Pos (State));

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
         end case;

         if not Self.Read (Parse_Array'Access, Array_State'Pos (State)) then
            if Self.Stream.Is_End_Of_Stream then
               if State = Finish then
                  return True;

               else
                  return Self.Report_Error ("unexpected end of document");
               end if;

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
                        Self.Push
                          (Parse_Array'Access, Array_State'Pos (State));

                        return False;
                     end if;

                     raise Program_Error;

                  when End_Array =>
                     State := Finish;
                     Self.Event := VSS.JSON.Pull_Readers.End_Array;
                     Self.Push (Parse_Array'Access, Array_State'Pos (State));

                     return False;

                  when others =>
                     return Self.Report_Error ("value or end array expected");
               end case;

            when Value =>
               State := Value_Separator_Or_End_Array;

               if not Self.Parse_Value then
                  Self.Push (Parse_Array'Access, Array_State'Pos (State));

                  return False;
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
               if Self.Event /= VSS.JSON.Pull_Readers.Invalid
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
         Self.Event := VSS.JSON.Pull_Readers.Start_Document;
         Self.Push (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

         return False;
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

                  when Wide_Wide_Character'Last =>
                     null;

                  when others =>
                     State := Done;
                     Self.Push
                       (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));

                     return Self.Report_Error ("end of document expected");
               end case;

            when Done =>
               Self.Event := VSS.JSON.Pull_Readers.End_Document;

               return True;
         end case;

         if not Self.Read
           (Parse_JSON_Text'Access, JSON_Text_State'Pos (State))
         then
            if Self.Stream.Is_End_Of_Stream then
               Self.Event := VSS.JSON.Pull_Readers.End_Document;

               return True;
            end if;

            return False;
         end if;

         case State is
            when Initial =>
               if not Self.Parse_Value then
                  if Self.Event /= VSS.JSON.Pull_Readers.Invalid
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

            when others =>
               raise Program_Error with JSON_Text_State'Image (State);
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
      Exp_Digits);

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
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
               Self.Number_State.Minus := True;

            when Digit_Zero =>
               State := Frac_Or_Exp;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));

            when Digit_One .. Digit_Nine =>
               State := Int_Digits;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
               VSS.JSON.Implementation.Numbers.Int_Digit
                 (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

            when others =>
               raise Program_Error;
         end case;
      end if;

      loop
         if not Self.Read (Parse_Number'Access, Number_State'Pos (State)) then
            if Self.Stream.Is_End_Of_Stream then
               if State
                    in Int_Digits | Frac_Or_Exp | Frac_Digits | Exp_Digits
                  --  XXX allowed states and conditions need to be checked.
               then
                  --  Simulate successful read when 'string' parsing has been
                  --  finished, 'string' is not nested into another construct,
                  --  and end of stream has been reached.

                  Self.C := Wide_Wide_Character'Last;

                  VSS.JSON.Implementation.Numbers.To_JSON_Number
                    (Self.Number_State,
                     Self.String_Value,
                     Self.Number);

                  return True;

               else
                  --  XXX Self.Stack.Push???

                  raise Program_Error;
               end if;

            else
               return False;
            end if;
         end if;

         case State is
            when Int =>
               case Self.C is
                  when Digit_Zero =>
                     State := Frac_Or_Exp;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when Digit_One .. Digit_Nine =>
                     State := Int_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Int_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when others =>
                     return Self.Report_Error ("digit expected");
               end case;

            when Int_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Int_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when Decimal_Point =>
                     State := Frac_Digit;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Decimal_Point
                       (Self.Number_State);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     VSS.JSON.Implementation.Numbers.To_JSON_Number
                       (Self.Number_State,
                        Self.String_Value,
                        Self.Number);

                     return True;
               end case;

            when Frac_Or_Exp =>
               case Self.C is
                  when Decimal_Point =>
                     State := Frac_Digit;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Decimal_Point
                       (Self.Number_State);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     VSS.JSON.Implementation.Numbers.To_JSON_Number
                       (Self.Number_State,
                        Self.String_Value,
                        Self.Number);

                     return True;
               end case;

            when Frac_Digit =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Frac_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Frac_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when others =>
                     return Self.Report_Error ("frac digit expected");
               end case;

            when Frac_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Frac_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Exp_Sign_Or_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     VSS.JSON.Implementation.Numbers.To_JSON_Number
                       (Self.Number_State,
                        Self.String_Value,
                        Self.Number);

                     return True;
               end case;

            when Exp_Sign_Or_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Exp_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when Hyphen_Minus =>
                     State := Exp_Digit;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     Self.Number_State.Exp_Minus := True;

                  when Plus_Sign =>
                     State := Exp_Digit;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     return Self.Report_Error ("plus/minus or digit expected");
               end case;

            when Exp_Digit =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Exp_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when others =>
                     return Self.Report_Error ("exp digit expected");
               end case;

            when Exp_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when others =>
                     VSS.JSON.Implementation.Numbers.To_JSON_Number
                       (Self.Number_State,
                        Self.String_Value,
                        Self.Number);

                     return True;
               end case;
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
               Self.Push (Parse_Object'Access, Object_State'Pos (State));

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
               Self.Event := VSS.JSON.Pull_Readers.Start_Object;
               Self.Push (Parse_Object'Access, Object_State'Pos (State));

               return False;

            when Member_Or_End_Object =>
               null;

            when Whitespace_Or_Member =>
               null;

            when Member_String =>
               State := Member_Name_Separator;
               Self.Event := VSS.JSON.Pull_Readers.Key_Name;
               Self.Push (Parse_Object'Access, Object_State'Pos (State));

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
                     Self.Event := VSS.JSON.Pull_Readers.End_Object;
                     Self.Push (Parse_Object'Access, Object_State'Pos (State));

                     return False;

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
            if Self.Stream.Is_End_Of_Stream then
               if State = Finish then
                  return True;

               else
                  return Self.Report_Error ("unexpected end of document");
               end if;

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
                        Self.Push
                          (Parse_Object'Access, Object_State'Pos (State));

                        return False;
                     end if;

                  when End_Object =>
                     State := Finish;
                     Self.Event := VSS.JSON.Pull_Readers.End_Object;
                     Self.Push (Parse_Object'Access, Object_State'Pos (State));

                     return False;

                  when others =>
                     return
                       Self.Report_Error ("string or end object expected");
               end case;

            when Member_String =>
               raise Program_Error;

            when Member_Name_Separator =>
               null;

            when Member_Value =>
               if not Self.Parse_Value then
                  State := Value_Separator_Or_End_Object;
                  Self.Push (Parse_Object'Access, Object_State'Pos (State));

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
                        Self.Push
                          (Parse_Object'Access, Object_State'Pos (State));

                        return False;
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

   function Parse_String (Self : in out JSON_Parser'Class) return Boolean is

      use type VSS.Unicode.Code_Point;
      use type VSS.Unicode.UTF16_Code_Unit;

      function Hex_To_Code
        (Code : in out VSS.Unicode.UTF16_Code_Unit) return Boolean;

      -----------------
      -- Hex_To_Code --
      -----------------

      function Hex_To_Code
        (Code : in out VSS.Unicode.UTF16_Code_Unit) return Boolean is
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
         Self.Buffer.Clear;
      end if;

      loop
         if not Self.Read (Parse_String'Access, String_State'Pos (State)) then
            if Self.Stream.Is_End_Of_Stream then
               if State = Finish then
                  return True;

               else
                  return Self.Report_Error ("premature end of string");
               end if;

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
                     return Self.Report_Error ("unescaped control character");

                  when Reverse_Solidus =>
                     State := Escape;

                  when others =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
               end case;

            when Escape =>
               case Self.C is
                  when Quotation_Mark =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Quotation_Mark));

                  when Reverse_Solidus =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Reverse_Solidus));

                  when Solidus =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Solidus));

                  when Latin_Small_Letter_B =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Backspace));

                  when Latin_Small_Letter_F =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Form_Feed));

                  when Latin_Small_Letter_N =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Line_Feed));

                  when Latin_Small_Letter_R =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Carriage_Return));

                  when Latin_Small_Letter_T =>
                     State := Character_Data;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character
                          (Character_Tabulation));

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
                  Self.Buffer.Append
                    (VSS.Characters.Virtual_Character'Val (Self.Code_Unit_1));

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

               declare
                  Code : VSS.Unicode.Code_Point := 16#01_0000#;

               begin
                  Code :=
                    Code
                      + VSS.Unicode.Code_Point
                         (Self.Code_Unit_1 and 16#03FF#) * 16#0400#
                      + VSS.Unicode.Code_Point
                         (Self.Code_Unit_2 and 16#03FF#);
                  Self.Buffer.Append
                    (VSS.Characters.Virtual_Character'Val (Code));
               end;

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
      Value_Number,
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

   function Parse_Value (Self : in out JSON_Parser'Class) return Boolean is
      State : Value_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Value_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               Self.Push (Parse_Value'Access, Value_State'Pos (State));

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
                        Self.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;

                     else
                        Self.Event := VSS.JSON.Pull_Readers.String_Value;

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
                        Self.Push
                          (Parse_Value'Access, Value_State'Pos (State));

                        return False;

                     else
                        Self.Event := VSS.JSON.Pull_Readers.Number_Value;

                        return False;
                     end if;

                  when Begin_Array =>
                     if not Self.Parse_Array then
                        return False;

                     else
                        --  Parse_Array always returns False for the first
                        --  call: it reports Start_Array event.

                        raise Program_Error;
                     end if;

                  when Begin_Object =>
                     if not Self.Parse_Object then
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
               Self.Event := VSS.JSON.Pull_Readers.String_Value;

               return False;

            when Value_Number =>
               Self.Event := VSS.JSON.Pull_Readers.Number_Value;

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
            if Self.Stream.Is_End_Of_Stream then
               if State = Finish then
                  return True;

               else
                  return Self.Report_Error ("premature end of value");
               end if;

            else
               return False;
            end if;
         end if;

         case State is
            when Initial =>
               null;

            when Value_Number =>
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
                     Self.Event := VSS.JSON.Pull_Readers.Boolean_Value;
                     Self.Push (Parse_Value'Access, Value_State'Pos (State));

                     return False;

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
                     Self.Event := VSS.JSON.Pull_Readers.Null_Value;
                     Self.Push (Parse_Value'Access, Value_State'Pos (State));

                     return False;

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
                     Self.Event := VSS.JSON.Pull_Readers.Boolean_Value;
                     Self.Push (Parse_Value'Access, Value_State'Pos (State));

                     return False;

                  when others =>
                     return Self.Report_Error ("true expected");
               end case;

            when Finish =>
               return True;
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
     (Self  : in out JSON_Parser'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) is
   begin
      if Self.Event /= VSS.JSON.Pull_Readers.Invalid
        or else Self.Error /= VSS.JSON.Pull_Readers.Not_Valid
      then
         Self.Stack.Push (Parse, State);
      end if;
   end Push;

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
      Character : VSS.Characters.Virtual_Character;

   begin
      Self.Stream.Get (Character, Success);

      if not Success then
         if Self.Stream.Is_End_Of_Stream then
            Self.C := Wide_Wide_Character'Last;
         end if;

         if Self.Stream.Has_Error then
            --  In case of IO error save error message and mark document as
            --  invalid.

            Self.Message := Self.Stream.Error_Message;
            Self.Event   := VSS.JSON.Pull_Readers.Invalid;
            Self.Error   := VSS.JSON.Pull_Readers.Not_Valid;

            return False;

         else
            Self.Event := VSS.JSON.Pull_Readers.Invalid;
            Self.Error := VSS.JSON.Pull_Readers.Premature_End_Of_Document;
         end if;

         if not Self.Stream.Is_End_Of_Stream then
            Self.Push (Parse, State);
         end if;

         return False;

      else
         Self.C := Wide_Wide_Character (Character);
      end if;

      return True;
   end Read;

   ------------------
   -- Report_Error --
   ------------------

   function Report_Error
     (Self    : in out JSON_Parser'Class;
      Message : Wide_Wide_String) return Boolean is
   begin
      Self.Event := VSS.JSON.Pull_Readers.Invalid;
      Self.Error := VSS.JSON.Pull_Readers.Not_Valid;
      Self.Message := VSS.Strings.To_Virtual_String (Message);

      return False;
   end Report_Error;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Parser'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Stream := Stream;
   end Set_Stream;

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (Self : JSON_Parser'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Buffer;
   end String_Value;

   ---------
   -- Top --
   ---------

   function Top (Self : Parse_Stack'Class) return Parse_State is
   begin
      return Self.Stack (Self.Head);
   end Top;

end VSS.JSON.Implementation.Parsers;
