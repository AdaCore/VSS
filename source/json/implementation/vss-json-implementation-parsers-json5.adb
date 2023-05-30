--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  RFC 8259 "The JavaScript Object Notation (JSON) Data Interchange Format"

with Ada.Unchecked_Conversion;

with VSS.Characters;
with VSS.Implementation.UCD_Core;

package body VSS.JSON.Implementation.Parsers.JSON5 is

   use type VSS.JSON.Pull_Readers.JSON_Event_Kind;
   use type VSS.JSON.Pull_Readers.JSON_Reader_Error;

   function Parse_JSON_Text
     (Self : in out JSON_Parser_Base'Class) return Boolean;
   --  Parse 'json-text'.

   function Parse_Value (Self : in out JSON_Parser_Base'Class) return Boolean;
   --  Parse 'value'. Skip all leading whitespaces.

   function Parse_Array (Self : in out JSON_Parser_Base'Class) return Boolean;

   function Parse_Object (Self : in out JSON_Parser_Base'Class) return Boolean;

   function Parse_Comment
     (Self : in out JSON_Parser_Base'Class) return Boolean;
   --  Parse comment.

   function Parse_Number
     (Parser : in out JSON_Parser_Base'Class) return Boolean
     with Post => Parse_Number'Result = False;
   --  Parse number. When parse of number is done Number_Value event is
   --  reported, thus, subprogram returns False always.

   function Parse_String
     (Parser : in out JSON_Parser_Base'Class) return Boolean;

   function Parse_Identifier
     (Self : in out JSON_Parser_Base'Class) return Boolean
     with Post => Parse_Identifier'Result = False;
   --  Parse JSON5Identifier. Emit Key_Name event when pasring is done, thus is
   --  never return True.

   function Parse_Unicode_Escape_Sequence
     (Parser : in out JSON_Parser_Base'Class) return Boolean;
   --  Parses UnicodeEspaceSequence production.

   function Is_Space_Separator (Self : JSON_Parser_Base'Class) return Boolean;
   --  Returns True when current character belongs to Zs (space, separator)
   --  general category.

   function Is_Unicode_Letter (Self : JSON_Parser_Base'Class) return Boolean;
   --  Returns True when current character belongs to Lu (uppercase letter),
   --  Ll (lowercase letter), Lt (titlecase letter), Lm (modifier letter), Lo
   --  (other latter), Nl (letternumber) categories.

   function Is_Identifier_Part (Self : JSON_Parser_Base'Class) return Boolean;
   --  Returns True when current character belongs to Lu (uppercase letter),
   --  Ll (lowercase letter), Lt (titlecase letter), Lm (modifier letter), Lo
   --  (other latter), Nl (letternumber), Mn (non-spacing mark), Mc (combining
   --  spacing mark), Nd (decimal number), Pc (connector punctuation)
   --  categories.

   function Hex_To_Code
     (Self : JSON5_Parser'Class;
      Code : in out VSS.Unicode.UTF16_Code_Unit) return Boolean;
   --  Converts current hexadecimal digit to numeric value and recomputes
   --  character's code, and returns True. Returns False when current
   --  character is not hexadecimal character.

   function Extract_Core_Data
     (Code : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Core.Core_Data_Record;
   --  Retrieve core properties record for the given code point.

   Nul                       : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_0000#);
   Backspace                 : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_0008#);
   Character_Tabulation      : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_0009#);
   Line_Feed                 : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_000A#);
   Line_Tabulation           : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_000B#);
   Form_Feed                 : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_000C#);
   Carriage_Return           : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_000D#);
   Space                     : constant Wide_Wide_Character := ' ';  --  U+0020
   Quotation_Mark            : constant Wide_Wide_Character := '"';  --  U+0022
   Dollar_Sign               : constant Wide_Wide_Character := '$';  --  U+0024
   Asterisk                  : constant Wide_Wide_Character := '*';  --  U+002A
   Plus_Sign                 : constant Wide_Wide_Character := '+';  --  U+002B
   Hyphen_Minus              : constant Wide_Wide_Character := '-';  --  U+002D
   Apostrophe                : constant Wide_Wide_Character := ''';  --  U+0027
   Solidus                   : constant Wide_Wide_Character := '/';  --  U+002F
   Digit_Zero                : constant Wide_Wide_Character := '0';
   Digit_One                 : constant Wide_Wide_Character := '1';
   Digit_Nine                : constant Wide_Wide_Character := '9';
   Latin_Capital_Letter_A    : constant Wide_Wide_Character := 'A';  --  U+0041
   Latin_Capital_Letter_E    : constant Wide_Wide_Character := 'E';  --  U+0045
   Latin_Capital_Letter_F    : constant Wide_Wide_Character := 'F';  --  U+0046
   Latin_Capital_Letter_I    : constant Wide_Wide_Character := 'I';  --  U+0049
   Latin_Capital_Letter_N    : constant Wide_Wide_Character := 'N';  --  U+004E
   Latin_Capital_Letter_X    : constant Wide_Wide_Character := 'X';  --  U+0058
   Reverse_Solidus           : constant Wide_Wide_Character := '\';  --  U+005C
   Low_Line                  : constant Wide_Wide_Character := '_';  --  U+005F
   Latin_Small_Letter_A      : constant Wide_Wide_Character := 'a';
   Latin_Small_Letter_B      : constant Wide_Wide_Character := 'b';  --  U+0062
   Latin_Small_Letter_E      : constant Wide_Wide_Character := 'e';
   Latin_Small_Letter_F      : constant Wide_Wide_Character := 'f';  --  U+0066
   Latin_Small_Letter_I      : constant Wide_Wide_Character := 'i';  --  U+0069
   Latin_Small_Letter_L      : constant Wide_Wide_Character := 'l';  --  U+006C
   Latin_Small_Letter_N      : constant Wide_Wide_Character := 'n';  --  U+006E
   Latin_Small_Letter_R      : constant Wide_Wide_Character := 'r';  --  U+0072
   Latin_Small_Letter_S      : constant Wide_Wide_Character := 's';  --  U+0071
   Latin_Small_Letter_T      : constant Wide_Wide_Character := 't';  --  U+0074
   Latin_Small_Letter_U      : constant Wide_Wide_Character := 'u';  --  U+0075
   Latin_Small_Letter_V      : constant Wide_Wide_Character := 'v';  --  U+0076
   Latin_Small_Letter_X      : constant Wide_Wide_Character := 'x';  --  U+0078
   Latin_Small_Letter_Y      : constant Wide_Wide_Character := 'y';  --  U+0079

   Begin_Array               : constant Wide_Wide_Character := '[';
   Begin_Object              : constant Wide_Wide_Character := '{';
   End_Array                 : constant Wide_Wide_Character := ']';
   End_Object                : constant Wide_Wide_Character := '}';
   Name_Separator            : constant Wide_Wide_Character := ':';
   Value_Separator           : constant Wide_Wide_Character := ',';
   Decimal_Point             : constant Wide_Wide_Character := '.';

   No_Break_Space            : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_00A0#);

   Zero_Width_Non_Joiner     : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_200C#);
   Zero_Width_Joiner         : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_200D#);

   Line_Separator            : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_2028#);
   Paragraph_Separator       : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_2029#);

   Zero_Width_No_Break_Space : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#00_FEFF#);

   -----------------------
   -- Extract_Core_Data --
   -----------------------

   function Extract_Core_Data
     (Code : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Core.Core_Data_Record
   is
      use type VSS.Implementation.UCD_Core.Core_Offset;
      use type VSS.Unicode.Code_Point;

      Block : constant VSS.Implementation.UCD_Core.Core_Index :=
        VSS.Implementation.UCD_Core.Core_Index
          (Code / VSS.Implementation.UCD_Core.Block_Size);
      Offset : constant VSS.Implementation.UCD_Core.Core_Offset :=
        VSS.Implementation.UCD_Core.Core_Offset
          (Code mod VSS.Implementation.UCD_Core.Block_Size);

   begin
      return
        VSS.Implementation.UCD_Core.Core_Data_Table
          (VSS.Implementation.UCD_Core.Core_Index_Table (Block) + Offset);
   end Extract_Core_Data;

   -----------------
   -- Hex_To_Code --
   -----------------

   function Hex_To_Code
     (Self : JSON5_Parser'Class;
      Code : in out VSS.Unicode.UTF16_Code_Unit) return Boolean
   is
      use type VSS.Unicode.UTF16_Code_Unit;

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

   ------------------------
   -- Is_Identifier_Part --
   ------------------------

   function Is_Identifier_Part
     (Self : JSON_Parser_Base'Class) return Boolean
   is
      use all type VSS.Implementation.UCD_Core.GC_Values;

   begin
      return
        Extract_Core_Data (Wide_Wide_Character'Pos (Self.C)).GC
      in GC_Lu | GC_Ll | GC_Lt | GC_Lm | GC_Lo | GC_Nl | GC_Mn | GC_Mc
          | GC_Nd | GC_Pc;
   end Is_Identifier_Part;

   ------------------------
   -- Is_Space_Separator --
   ------------------------

   function Is_Space_Separator
     (Self : JSON_Parser_Base'Class) return Boolean
   is
      use all type VSS.Implementation.UCD_Core.GC_Values;

   begin
      return Extract_Core_Data (Wide_Wide_Character'Pos (Self.C)).GC = GC_Zs;
   end Is_Space_Separator;

   -----------------------
   -- Is_Unicode_Letter --
   -----------------------

   function Is_Unicode_Letter (Self : JSON_Parser_Base'Class) return Boolean is
      use all type VSS.Implementation.UCD_Core.GC_Values;

   begin
      return
        Extract_Core_Data (Wide_Wide_Character'Pos (Self.C)).GC
          in GC_Lu | GC_Ll | GC_Lt | GC_Lm | GC_Lo | GC_Nl;
   end Is_Unicode_Letter;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self : in out JSON5_Parser'Class) is
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
      Value_Separator_Or_End_Array,
      Finish);

   function Parse_Array
     (Self : in out JSON_Parser_Base'Class) return Boolean
   is
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
               return Self.Push (Parse_Array'Access, Array_State'Pos (State));
            end if;
         end if;

      else
         pragma Assert (Self.C = Begin_Array);

         State := Value_Or_End_Array;
         Self.Event := VSS.JSON.Pull_Readers.Start_Array;

         return Self.Push (Parse_Array'Access, Array_State'Pos (State));
      end if;

      loop
         case State is
            when Value_Or_End_Array =>
               null;

            when Value_Separator_Or_End_Array =>
               case Self.C is
                  when Character_Tabulation
                     | Line_Feed
                     | Line_Tabulation
                     | Form_Feed
                     | Carriage_Return
                     | Space
                     | No_Break_Space
                     | Line_Separator
                     | Paragraph_Separator
                     | Zero_Width_No_Break_Space
                  =>
                     null;

                  when Solidus =>
                     if not Parse_Comment (Self) then
                        return
                          Self.Push
                            (Parse_Array'Access, Array_State'Pos (State));
                     end if;

                  when Value_Separator =>
                     State := Value_Or_End_Array;

                  when End_Array =>
                     State := Finish;
                     Self.Event := VSS.JSON.Pull_Readers.End_Array;

                     return
                       Self.Push (Parse_Array'Access, Array_State'Pos (State));

                  when End_Of_Stream =>
                     return Self.Report_Error ("unexpected end of document");

                  when others =>
                     if not Is_Space_Separator (Self) then
                        return
                          Self.Report_Error
                            ("value separator or end array expected");
                     end if;
               end case;

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
                  when Character_Tabulation
                     | Line_Feed
                     | Line_Tabulation
                     | Form_Feed
                     | Carriage_Return
                     | Space
                     | No_Break_Space
                     | Line_Separator
                     | Paragraph_Separator
                     | Zero_Width_No_Break_Space
                  =>
                     null;

                  when Solidus =>
                     raise Program_Error;

                  when Begin_Array
                     | Begin_Object
                     | Quotation_Mark
                     | Apostrophe
                     | Plus_Sign
                     | Hyphen_Minus
                     | Decimal_Point
                     | Digit_Zero .. Digit_Nine
                     | Latin_Capital_Letter_I
                     | Latin_Capital_Letter_N
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
                     Self.Event := VSS.JSON.Pull_Readers.End_Array;

                     return
                       Self.Push (Parse_Array'Access, Array_State'Pos (State));

                  when End_Of_Stream =>
                     raise Program_Error;

                  when others =>
                     if not Is_Space_Separator (Self) then
                        return
                          Self.Report_Error ("value or end array expected");
                     end if;
               end case;

            when Value_Separator_Or_End_Array =>
               null;
               --  raise Program_Error;

            when Finish =>
               return True;
         end case;
      end loop;
   end Parse_Array;

   -------------------
   -- Parse_Comment --
   -------------------

   type Comment_State is
     (Comment,
      Single_Line,
      Multi_Line,
      Multi_Line_Asterisk,
      Multi_Line_Done);

   function Parse_Comment
     (Self : in out JSON_Parser_Base'Class) return Boolean
   is
      State : Comment_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Comment_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            raise Program_Error;
         end if;

      else
         pragma Assert (Self.C = Solidus);

         State := Comment;
      end if;

      loop
         if not Self.Read
                  (Parse_Comment'Access, Comment_State'Pos (State))
         then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State in Single_Line | Multi_Line_Done then
               return True;

            else
               return Self.Report_Error ("unexpected end of document");
            end if;
         end if;

         case State is
            when Comment =>
               case Self.C is
                  when Solidus =>
                     State := Single_Line;

                  when Asterisk =>
                     State := Multi_Line;

                  when others =>
                     raise Program_Error;
               end case;

            when Single_Line =>
               case Self.C is
                  when Line_Feed
                     | Carriage_Return
                     | Line_Separator
                     | Paragraph_Separator
                  =>
                     return True;

                  when others =>
                     null;
               end case;

            when Multi_Line =>
               case Self.C is
                  when Asterisk =>
                     State := Multi_Line_Asterisk;

                  when others =>
                     null;
               end case;

            when Multi_Line_Asterisk =>
               case Self.C is
                  when Solidus =>
                     State := Multi_Line_Done;

                  when Asterisk =>
                     null;

                  when others =>
                     State := Multi_Line;
               end case;

            when Multi_Line_Done =>
               return True;
         end case;
      end loop;
   end Parse_Comment;

   ----------------------
   -- Parse_Identifier --
   ----------------------

   type Identifier_State is
     (Identifier_Part,
      Escape,
      Report_Key_Name);

   function Parse_Identifier
     (Self : in out JSON_Parser_Base'Class) return Boolean
   is
      --  JSON5Identifier::
      --    IdentifierName
      --
      --  IdentifierName ::
      --    IdentifierStart
      --    IdentifierName IdentifierPart
      --
      --  IdentifierStart ::
      --    UnicodeLetter
      --    $
      --    _
      --    \ UnicodeEscapeSequence
      --
      --  IdentifierPart ::
      --    IdentifierStart
      --    UnicodeCombiningMark
      --    UnicodeDigit
      --    UnicodeConnectorPunctuation
      --    <ZWNJ>
      --    <ZWJ>

      State : Identifier_State;

   begin
      if not Self.Stack.Is_Empty then
         State := Identifier_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               return
                 Self.Push
                   (Parse_Identifier'Access, Identifier_State'Pos (State));
            end if;
         end if;

      else
         pragma Assert
                 (Self.C in Dollar_Sign | Low_Line | Reverse_Solidus
                    or Is_Unicode_Letter (Self));

         Self.Buffer.Clear;

         case Self.C is
            when Dollar_Sign | Low_Line =>
               State := Identifier_Part;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));

            when Reverse_Solidus =>
               State := Escape;

               raise Program_Error;

            when others =>
               pragma Assert (Is_Unicode_Letter (Self));

               State := Identifier_Part;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
         end case;
      end if;

      loop
         case State is
            when Report_Key_Name =>
               Self.Event := VSS.JSON.Pull_Readers.Key_Name;

               return False;

            when others =>
               null;
         end case;

         if not Self.Read
                  (Parse_Identifier'Access, Identifier_State'Pos (State))
         then
            if not Self.Stream.Is_End_Of_Stream then
               return False;
            end if;

            State := Report_Key_Name;
         end if;

         case State is
            when Identifier_Part =>
               case Self.C is
                  when Dollar_Sign
                     | Low_Line
                     | Zero_Width_Non_Joiner
                     | Zero_Width_Joiner
                  =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when Reverse_Solidus =>
                     State := Escape;

                  when others =>
                     if Is_Identifier_Part (Self) then
                        Self.Buffer.Append
                          (VSS.Characters.Virtual_Character (Self.C));

                     else
                        State := Report_Key_Name;
                     end if;
               end case;

            when Escape =>
               case Self.C is
                  when Latin_Small_Letter_U =>
                     State := Identifier_Part;

                     if not Parse_Unicode_Escape_Sequence (Self) then
                        return
                          Self.Push
                            (Parse_Identifier'Access,
                             Identifier_State'Pos (State));
                     end if;

                  when others =>
                     raise Program_Error;
               end case;

            when Report_Key_Name =>
               null;
         end case;
      end loop;
   end Parse_Identifier;

   ---------------------
   -- Parse_JSON_Text --
   ---------------------

   type JSON_Text_State is (Initial, Whitespace_Or_End, Done);

   function Parse_JSON_Text
     (Self : in out JSON_Parser_Base'Class) return Boolean
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

         return
           Self.Push (Parse_JSON_Text'Access, JSON_Text_State'Pos (State));
      end if;

      <<Restart>>

      loop
         case State is
            when Initial =>
               null;

            when Whitespace_Or_End =>
               if Self.Event = VSS.JSON.Pull_Readers.Invalid
                 and then Self.Error = VSS.JSON.Pull_Readers.Not_Valid
               then
                  State := Done;

                  goto Restart;
               end if;

               case Self.C is
                  when Character_Tabulation
                     | Line_Feed
                     | Line_Tabulation
                     | Form_Feed
                     | Carriage_Return
                     | Space
                     | No_Break_Space
                     | Line_Separator
                     | Paragraph_Separator
                     | Zero_Width_No_Break_Space
                  =>
                     null;

                  when Solidus =>
                     if not Parse_Comment (Self) then
                        Self.Stack.Push
                          (Parse_JSON_Text'Access,
                           JSON_Text_State'Pos (State));

                        return False;

                     else
                        goto Restart;
                     end if;

                  when End_Of_Stream =>
                     Self.Event := VSS.JSON.Pull_Readers.End_Document;

                     return False;

                  when others =>
                     if not Is_Space_Separator (Self) then
                        State := Done;
                        Self.Stack.Push
                          (Parse_JSON_Text'Access,
                           JSON_Text_State'Pos (State));

                        return Self.Report_Error ("end of document expected");
                     end if;
               end case;

            when Done =>
               Self.Event := VSS.JSON.Pull_Readers.End_Document;

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
               State := Whitespace_Or_End;

               if not Parse_Value (Self) then
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
     (JSON5_Numeric_Literal,
      Numeric_0,
      Decimal_Integral_Digits_Opt,
      Decimal_Fraction_Digits,
      Decimal_Fraction_Digits_Opt,
      Decimal_Exponent_Signed_Integer,
      Decimal_Exponent_Digits,
      Decimal_Exponent_Digits_Opt,
      Hex_Digits,
      Hex_Digits_Opt,
      Number_I,
      Number_IN,
      Number_INF,
      Number_INFI,
      Number_INFIN,
      Number_INFINI,
      Number_INFINIT,
      Number_N,
      Number_NA,
      Report_Decimal_Value,
      Report_Hex_Value,
      Report_Special_Value);

   function Parse_Number
     (Parser : in out JSON_Parser_Base'Class) return Boolean
   is
      --  JSON5Number::
      --    JSON5NumericLiteral
      --    + JSON5NumericLiteral
      --    - JSON5NumericLiteral
      --
      --  JSON5NumericLiteral::
      --    NumericLiteral
      --    Infinity
      --    NaN
      --
      --  NumericLiteral ::
      --    DecimalLiteral
      --    HexIntegerLiteral
      --
      --  DecimalLiteral ::
      --    DecimalIntegerLiteral . DecimalDigitsopt ExponentPartopt
      --    . DecimalDigits ExponentPartopt
      --    DecimalIntegerLiteral ExponentPartopt
      --
      --  DecimalIntegerLiteral ::
      --    0
      --    NonZeroDigit DecimalDigitsopt
      --
      --  DecimalDigits ::
      --    DecimalDigit
      --    DecimalDigits DecimalDigit
      --
      --  DecimalDigit :: one of
      --    0 1 2 3 4 5 6 7 8 9
      --
      --  NonZeroDigit :: one of
      --    1 2 3 4 5 6 7 8 9
      --
      --  ExponentPart ::
      --    ExponentIndicator SignedInteger
      --
      --  ExponentIndicator :: one of
      --    e E
      --
      --  SignedInteger ::
      --    DecimalDigits
      --    + DecimalDigits
      --    - DecimalDigits
      --
      --  HexIntegerLiteral ::
      --    0x HexDigit
      --    0X HexDigit
      --    HexIntegerLiteral HexDigit
      --
      --  HexDigit :: one of
      --    0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F

      use type Interfaces.Integer_64;
      use type Interfaces.Unsigned_64;

      State : Number_State;
      Self  : JSON5_Parser'Class renames JSON5_Parser'Class (Parser);

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
            when Plus_Sign =>
               State := JSON5_Numeric_Literal;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
               Self.Number_State.Minus := False;

            when Hyphen_Minus =>
               State := JSON5_Numeric_Literal;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
               Self.Number_State.Minus := True;

            when Decimal_Point =>
               State := Decimal_Fraction_Digits;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
               VSS.JSON.Implementation.Numbers.Decimal_Point
                 (Self.Number_State);

            when Digit_Zero =>
               State := Numeric_0;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));

            when Digit_One .. Digit_Nine =>
               State := Decimal_Integral_Digits_Opt;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
               VSS.JSON.Implementation.Numbers.Int_Digit
                 (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

            when Latin_Capital_Letter_I =>
               State := Number_I;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));

            when Latin_Capital_Letter_N =>
               State := Number_N;
               Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));

            when others =>
               raise Program_Error;
         end case;
      end if;

      loop
         case State is
            when Report_Decimal_Value =>
               VSS.JSON.Implementation.Numbers.To_JSON_Number
                 (Self.Number_State,
                  Self.String_Value,
                  Self.Number);
               Self.Event := VSS.JSON.Pull_Readers.Number_Value;

               return False;

            when Report_Hex_Value =>
               if (Self.Unsigned and 16#8000_0000_0000_0000#) = 0 then
                  Self.Number :=
                    (JSON_Integer,
                     Self.Buffer,
                     (if not Self.Number_State.Minus
                        then Interfaces.Integer_64 (Self.Unsigned)
                        else -Interfaces.Integer_64 (Self.Unsigned)));

               else
                  Self.Number := (Out_Of_Range, Self.Buffer);
               end if;

               Self.Event := VSS.JSON.Pull_Readers.Number_Value;

               return False;

            when others =>
               null;
         end case;

         if not Self.Read (Parse_Number'Access, Number_State'Pos (State)) then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State in Numeric_0
                             | Decimal_Integral_Digits_Opt
                             | Decimal_Fraction_Digits_Opt
                             | Decimal_Exponent_Digits_Opt
            then
               State := Report_Decimal_Value;

            else
               --  XXX Self.Stack.Push???

               raise Program_Error;
            end if;
         end if;

         case State is
            when JSON5_Numeric_Literal =>
               case Self.C is
                  when Decimal_Point =>
                     State := Decimal_Fraction_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Decimal_Point
                       (Self.Number_State);

                  when Digit_Zero =>
                     State := Numeric_0;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when Digit_One .. Digit_Nine =>
                     State := Decimal_Integral_Digits_Opt;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Int_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when Latin_Capital_Letter_I =>
                     State := Number_I;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when Latin_Capital_Letter_N =>
                     State := Number_N;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     return
                       Self.Report_Error
                         ("point, digit, Infinity or Nan expected");
               end case;

            when Numeric_0 =>
               case Self.C is
                  when Decimal_Point =>
                     State := Decimal_Fraction_Digits_Opt;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Decimal_Point
                       (Self.Number_State);

                  when Digit_Zero .. Digit_Nine =>
                     State := Report_Decimal_Value;

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Decimal_Exponent_Signed_Integer;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when Latin_Capital_Letter_X | Latin_Small_Letter_X =>
                     State := Hex_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     Self.Unsigned := 0;

                  when others =>
                     State := Report_Decimal_Value;
               end case;

            when Decimal_Integral_Digits_Opt =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Int_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when Decimal_Point =>
                     State := Decimal_Fraction_Digits_Opt;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Decimal_Point
                       (Self.Number_State);

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Decimal_Exponent_Signed_Integer;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     State := Report_Decimal_Value;
               end case;

            when Decimal_Fraction_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Decimal_Fraction_Digits_Opt;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Frac_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when others =>
                     return Self.Report_Error ("digit expected");
               end case;

            when Decimal_Fraction_Digits_Opt =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Frac_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when Latin_Capital_Letter_E | Latin_Small_Letter_E =>
                     State := Decimal_Exponent_Signed_Integer;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     State := Report_Decimal_Value;
               end case;

            when Decimal_Exponent_Signed_Integer =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Decimal_Exponent_Digits_Opt;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when Hyphen_Minus =>
                     State := Decimal_Exponent_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     Self.Number_State.Exp_Minus := True;

                  when Plus_Sign =>
                     State := Decimal_Exponent_Digits;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     return Self.Report_Error ("plus/minus or digit expected");
               end case;

            when Decimal_Exponent_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     State := Decimal_Exponent_Digits_Opt;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when others =>
                     return Self.Report_Error ("exp digit expected");
               end case;

            when Decimal_Exponent_Digits_Opt =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     VSS.JSON.Implementation.Numbers.Exp_Digit
                       (Self.Number_State, Wide_Wide_Character'Pos (Self.C));

                  when others =>
                     State := Report_Decimal_Value;
               end case;

            when Hex_Digits =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine
                     | Latin_Capital_Letter_A .. Latin_Capital_Letter_F
                     | Latin_Small_Letter_A .. Latin_Small_Letter_F
                  =>
                     declare
                        Code    : VSS.Unicode.UTF16_Code_Unit := 0;
                        Success : Boolean;

                     begin
                        State := Hex_Digits_Opt;
                        Self.Buffer.Append
                          (VSS.Characters.Virtual_Character (Self.C));
                        Success := Self.Hex_To_Code (Code);
                        pragma Assert (Success);

                        if (Self.Unsigned and 16#F800_0000_0000_0000#) = 0 then
                           Self.Unsigned :=
                             Interfaces.Shift_Left (@, 4)
                               + Interfaces.Unsigned_64 (Code);

                        else
                           Self.Unsigned := 16#FFFF_FFFF_FFFF_FFFF#;

                           raise Program_Error;
                        end if;
                     end;

                  when others =>
                     return Self.Report_Error ("hexadecimal digit expected");
               end case;

            when Hex_Digits_Opt =>
               case Self.C is
                  when Digit_Zero .. Digit_Nine
                     | Latin_Capital_Letter_A .. Latin_Capital_Letter_F
                     | Latin_Small_Letter_A .. Latin_Small_Letter_F
                  =>
                     declare
                        Code    : VSS.Unicode.UTF16_Code_Unit := 0;
                        Success : Boolean;

                     begin
                        Self.Buffer.Append
                          (VSS.Characters.Virtual_Character (Self.C));
                        Success := Self.Hex_To_Code (Code);
                        pragma Assert (Success);

                        if (Self.Unsigned and 16#F800_0000_0000_0000#) = 0 then
                           Self.Unsigned :=
                             Interfaces.Shift_Left (@, 4)
                               + Interfaces.Unsigned_64 (Code);

                        else
                           Self.Unsigned := 16#FFFF_FFFF_FFFF_FFFF#;

                           raise Program_Error;
                        end if;
                     end;

                  when others =>
                     State := Report_Hex_Value;
               end case;

            when Number_I =>
               case Self.C is
                  when Latin_Small_Letter_N =>
                     State := Number_IN;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     raise Program_Error;
               end case;

            when Number_IN =>
               case Self.C is
                  when Latin_Small_Letter_F =>
                     State := Number_INF;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     raise Program_Error;
               end case;

            when Number_INF =>
               case Self.C is
                  when Latin_Small_Letter_I =>
                     State := Number_INFI;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     return Self.Report_Error ("Infinity expected");
               end case;

            when Number_INFI =>
               case Self.C is
                  when Latin_Small_Letter_N =>
                     State := Number_INFIN;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     raise Program_Error;
               end case;

            when Number_INFIN =>
               case Self.C is
                  when Latin_Small_Letter_I =>
                     State := Number_INFINI;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     raise Program_Error;
               end case;

            when Number_INFINI =>
               case Self.C is
                  when Latin_Small_Letter_T =>
                     State := Number_INFINIT;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     raise Program_Error;
               end case;

            when Number_INFINIT =>
               case Self.C is
                  when Latin_Small_Letter_Y =>
                     State := Report_Special_Value;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     Self.Number := (Out_Of_Range, Self.Buffer);

                  when others =>
                     raise Program_Error;
               end case;

            when Number_N =>
               case Self.C is
                  when Latin_Small_Letter_A =>
                     State := Number_NA;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when others =>
                     raise Program_Error;
               end case;

            when Number_NA =>
               case Self.C is
                  when Latin_Capital_Letter_N =>
                     State := Report_Special_Value;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
                     Self.Number := (Out_Of_Range, Self.Buffer);

                  when others =>
                     raise Program_Error;
               end case;

            when Report_Special_Value =>
               Self.Event := VSS.JSON.Pull_Readers.Number_Value;

               return False;

            when Report_Decimal_Value | Report_Hex_Value =>
               null;
         end case;
      end loop;
   end Parse_Number;

   ------------------
   -- Parse_Object --
   ------------------

   type Object_State is
     (Member_Or_End_Object,
      Member_String,
      Member_Name_Separator,
      Member_Value,
      Value_Separator_Or_End_Object,
      Finish);

   function Parse_Object
     (Self : in out JSON_Parser_Base'Class) return Boolean
   is
      --  JSON5Object:
      --    { }
      --    { JSON5MemberList ,opt }
      --
      --  JSON5MemberList:
      --    JSON5Member
      --    JSON5MemberList , JSON5Member
      --
      --  JSON5Member:
      --    JSON5MemberName : JSON5Value
      --
      --  JSON5MemberName:
      --    JSON5Identifier
      --    JSON5String

      State   : Object_State;
      Success : Boolean;

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
         Self.Event := VSS.JSON.Pull_Readers.Start_Object;

         return Self.Push (Parse_Object'Access, Object_State'Pos (State));
      end if;

      loop
         case State is
            when Member_Or_End_Object =>
               null;

            when Member_String =>
               State := Member_Name_Separator;
               Self.Event := VSS.JSON.Pull_Readers.Key_Name;

               return
                 Self.Push (Parse_Object'Access, Object_State'Pos (State));

            when Member_Name_Separator =>
               case Self.C is
                  when Character_Tabulation
                     | Line_Feed
                     | Line_Tabulation
                     | Form_Feed
                     | Carriage_Return
                     | Space
                     | No_Break_Space
                     | Line_Separator
                     | Paragraph_Separator
                     | Zero_Width_No_Break_Space
                  =>
                     null;

                  when Solidus =>
                     raise Program_Error;

                  when Name_Separator =>
                     State := Member_Value;

                  when End_Of_Stream =>
                     return Self.Report_Error ("unexpected end of document");

                  when others =>
                     if not Is_Space_Separator (Self) then
                        return Self.Report_Error ("name separator expected");
                     end if;
               end case;

            when Value_Separator_Or_End_Object =>
               case Self.C is
                  when Character_Tabulation
                     | Line_Feed
                     | Line_Tabulation
                     | Form_Feed
                     | Carriage_Return
                     | Space
                     | No_Break_Space
                     | Line_Separator
                     | Paragraph_Separator
                     | Zero_Width_No_Break_Space
                  =>
                     null;

                  when Solidus =>
                     raise Program_Error;

                  when Value_Separator =>
                     State := Member_Or_End_Object;

                  when End_Object =>
                     State := Finish;
                     Self.Event := VSS.JSON.Pull_Readers.End_Object;

                     return
                       Self.Push
                         (Parse_Object'Access, Object_State'Pos (State));

                  when End_Of_Stream =>
                     return Self.Report_Error ("unexpected end of document");

                  when others =>
                     if not Is_Space_Separator (Self) then
                        return
                          Self.Report_Error
                            ("value separator or end object expected");
                     end if;
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
                  when Character_Tabulation
                     | Line_Feed
                     | Line_Tabulation
                     | Form_Feed
                     | Carriage_Return
                     | Space
                     | No_Break_Space
                     | Line_Separator
                     | Paragraph_Separator
                     | Zero_Width_No_Break_Space
                  =>
                     null;

                  when Solidus =>
                     if not Parse_Comment (Self) then
                        return
                          Self.Push
                            (Parse_Object'Access, Object_State'Pos (State));
                     end if;

                  when Quotation_Mark | Apostrophe =>
                     State := Member_String;

                     if not Parse_String (Self) then
                        return
                          Self.Push
                            (Parse_Object'Access, Object_State'Pos (State));
                     end if;

                  when End_Object =>
                     State := Finish;
                     Self.Event := VSS.JSON.Pull_Readers.End_Object;

                     return
                       Self.Push
                         (Parse_Object'Access, Object_State'Pos (State));

                  when Dollar_Sign | Low_Line | Reverse_Solidus =>
                     Success := Parse_Identifier (Self);
                     pragma Assert (not Success);  --  Always return False

                     State := Member_Name_Separator;

                     return
                       Self.Push
                         (Parse_Object'Access, Object_State'Pos (State));

                  when End_Of_Stream =>
                     raise Program_Error;

                  when others =>
                     if Is_Unicode_Letter (Self) then
                        Success := Parse_Identifier (Self);
                        pragma Assert (not Success);  --  Always return False

                        State := Member_Name_Separator;

                        return
                          Self.Push
                            (Parse_Object'Access, Object_State'Pos (State));

                     elsif not Is_Space_Separator (Self) then
                        return
                          Self.Report_Error
                            ("string, identifier or end object expected");
                     end if;
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

            when Finish =>
               return True;
         end case;
      end loop;
   end Parse_Object;

   ------------------
   -- Parse_String --
   ------------------

   type String_State is
     (Double_Character_Data,
      Single_Character_Data,
      Escape,
      Escape_CR,
      Escape_X,
      Escape_XX,
      Finish);

   type String_State_Record is record
      Current : String_State;
      Default : String_State;
   end record with Size => Interfaces.Unsigned_32'Size;

   function Parse_String
     (Parser : in out JSON_Parser_Base'Class) return Boolean
   is

      function To_String_State is
        new Ada.Unchecked_Conversion
              (Interfaces.Unsigned_32, String_State_Record);

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion
              (String_State_Record, Interfaces.Unsigned_32);

      State : String_State_Record;
      Self  : JSON5_Parser'Class renames JSON5_Parser'Class (Parser);

   begin
      if not Self.Stack.Is_Empty then
         State := To_String_State (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            if not Self.Stack.Top.Parse (Self) then
               return Self.Push (Parse_String'Access, To_Unsigned_32 (State));
            end if;
         end if;

      else
         pragma Assert (Self.C in Quotation_Mark | Apostrophe);

         if Self.C = Quotation_Mark then
            State.Default := Double_Character_Data;

         else
            State.Default := Single_Character_Data;
         end if;

         Self.Buffer.Clear;
         State.Current := State.Default;
      end if;

      loop
         if not Self.Read (Parse_String'Access, To_Unsigned_32 (State)) then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            elsif State.Current = Finish then
               return True;

            else
               return Self.Report_Error ("premature end of string");
            end if;
         end if;

         <<Restart>>

         case State.Current is
            when Double_Character_Data =>
               case Self.C is
                  when Quotation_Mark =>
                     State.Current := Finish;

                  when Line_Feed | Carriage_Return =>
                     return Self.Report_Error ("unescaped line terminator");

                  when Line_Separator | Paragraph_Separator =>
                     --  XXX JSON5: it is recommended to report warning
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when Reverse_Solidus =>
                     State.Current := Escape;

                  when others =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
               end case;

            when Single_Character_Data =>
               case Self.C is
                  when Apostrophe =>
                     State.Current := Finish;

                  when Line_Feed | Carriage_Return =>
                     return Self.Report_Error ("unescaped line terminator");

                  when Line_Separator | Paragraph_Separator =>
                     --  XXX JSON5: it is recommended to report warning
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

                  when Reverse_Solidus =>
                     State.Current := Escape;

                  when others =>
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));
               end case;

            when Escape =>
               case Self.C is
                  when Line_Feed
                     | Line_Separator
                     | Paragraph_Separator
                  =>
                     State.Current := State.Default;

                  when Carriage_Return =>
                     State.Current := Escape_CR;

                  when Quotation_Mark =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Quotation_Mark));

                  when Apostrophe =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Apostrophe));

                  when Reverse_Solidus =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Reverse_Solidus));

                  when Solidus =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Solidus));

                  when Digit_Zero =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Nul));

                  when Digit_One .. Digit_Nine =>
                     return
                       Self.Report_Error
                         ("invalid character in escape sequence");

                  when Latin_Small_Letter_B =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Backspace));

                  when Latin_Small_Letter_F =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Form_Feed));

                  when Latin_Small_Letter_N =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Line_Feed));

                  when Latin_Small_Letter_R =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Carriage_Return));

                  when Latin_Small_Letter_T =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character
                          (Character_Tabulation));

                  when Latin_Small_Letter_U =>
                     State.Current := State.Default;

                     if not Parse_Unicode_Escape_Sequence (Self) then
                        return
                          Self.Push
                            (Parse_String'Access, To_Unsigned_32 (State));
                     end if;

                  when Latin_Small_Letter_V =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Line_Tabulation));

                  when Latin_Small_Letter_X =>
                     State.Current := Escape_X;

                  when others =>
                     State.Current := State.Default;
                     Self.Buffer.Append
                       (VSS.Characters.Virtual_Character (Self.C));

               end case;

            when Escape_CR =>
               State.Current := State.Default;

               if Self.C /= Line_Feed then
                  goto Restart;
               end if;

            when Escape_X =>
               State.Current := Escape_XX;
               Self.Code_Unit_1 := 0;

               if not Self.Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_XX =>
               State.Current := State.Default;

               if not Self.Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

               Self.Buffer.Append
                 (VSS.Characters.Virtual_Character'Val (Self.Code_Unit_1));

            when Finish =>
               return True;
         end case;
      end loop;
   end Parse_String;

   -----------------------------------
   -- Parse_Unicode_Escape_Sequence --
   -----------------------------------

   type Unicode_Escape_Sequence_State is
     (Escape_U,
      Escape_UX,
      Escape_UXX,
      Escape_UXXX,
      Escape_UXXXX,
      Escape_UXXXX_Escape,
      Escape_UXXXX_Escape_U,
      Escape_UXXXX_Escape_UX,
      Escape_UXXXX_Escape_UXX,
      Escape_UXXXX_Escape_UXXX);

   function Parse_Unicode_Escape_Sequence
     (Parser : in out JSON_Parser_Base'Class) return Boolean
   is
      use type VSS.Unicode.Code_Point;
      use type VSS.Unicode.UTF16_Code_Unit;

      State : Unicode_Escape_Sequence_State;
      Self  : JSON5_Parser'Class renames JSON5_Parser'Class (Parser);

   begin
      if not Self.Stack.Is_Empty then
         State := Unicode_Escape_Sequence_State'Val (Self.Stack.Top.State);
         Self.Stack.Pop;

         if not Self.Stack.Is_Empty then
            raise Program_Error;
         end if;

      else
         pragma Assert (Self.C = Latin_Small_Letter_U);

         State := Escape_U;
         Self.Code_Unit_1 := 0;
         Self.Code_Unit_2 := 0;
      end if;

      loop
         if not Self.Read
           (Parse_Unicode_Escape_Sequence'Access,
            Unicode_Escape_Sequence_State'Pos (State))
         then
            if not Self.Stream.Is_End_Of_Stream then
               return False;

            else
               return
                 Self.Report_Error
                   ("premature end of unicode escape sequence");
            end if;
         end if;

         case State is
            when Escape_U =>
               State := Escape_UX;

               if not Self.Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UX =>
               State := Escape_UXX;

               if not Self.Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXX =>
               State := Escape_UXXX;

               if not Self.Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXX =>
               if not Self.Hex_To_Code (Self.Code_Unit_1) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

               if Self.Code_Unit_1 not in 16#D800# .. 16#DFFF# then
                  Self.Buffer.Append
                    (VSS.Characters.Virtual_Character'Val (Self.Code_Unit_1));

                  return True;

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

               if not Self.Hex_To_Code (Self.Code_Unit_2) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXXX_Escape_UX =>
               State := Escape_UXXXX_Escape_UXX;

               if not Self.Hex_To_Code (Self.Code_Unit_2) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXXX_Escape_UXX =>
               State := Escape_UXXXX_Escape_UXXX;

               if not Self.Hex_To_Code (Self.Code_Unit_2) then
                  return Self.Report_Error ("hexadecimal letter expected");
               end if;

            when Escape_UXXXX_Escape_UXXX =>
               if not Self.Hex_To_Code (Self.Code_Unit_2) then
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

               return True;
         end case;
      end loop;
   end Parse_Unicode_Escape_Sequence;

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

      <<Restart>>

      loop
         case State is
            when Initial =>
               case Self.C is
                  when Character_Tabulation
                     | Line_Feed
                     | Line_Tabulation
                     | Form_Feed
                     | Carriage_Return
                     | Space
                     | No_Break_Space
                     | Line_Separator
                     | Paragraph_Separator
                     | Zero_Width_No_Break_Space
                  =>
                     null;

                  when Solidus =>
                     if not Parse_Comment (Self) then
                        return
                          Self.Push
                            (Parse_Value'Access, Value_State'Pos (State));

                     else
                        goto Restart;
                     end if;

                  when Quotation_Mark | Apostrophe =>
                     if not Parse_String (Self) then
                        State := Value_String;

                        return
                          Self.Push
                            (Parse_Value'Access, Value_State'Pos (State));

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

                  when Plus_Sign
                     | Hyphen_Minus
                     | Decimal_Point
                     | Digit_Zero .. Digit_Nine
                     | Latin_Capital_Letter_I
                     | Latin_Capital_Letter_N
                  =>
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

                  when End_Of_Stream =>
                     return Self.Report_Error ("value expected");

                  when others =>
                     if not Is_Space_Separator (Self) then
                        return Self.Report_Error ("value expected");
                     end if;
               end case;

            when Value_String =>
               Self.Event := VSS.JSON.Pull_Readers.String_Value;

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

               elsif State = Initial then
                  return Self.Report_Error ("value expected");

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
                     Self.Event := VSS.JSON.Pull_Readers.Null_Value;

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
                     Self.Event := VSS.JSON.Pull_Readers.Boolean_Value;

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

end VSS.JSON.Implementation.Parsers.JSON5;
