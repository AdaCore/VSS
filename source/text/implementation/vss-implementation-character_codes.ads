--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Character codes to reuse around internal code.

package VSS.Implementation.Character_Codes is

   pragma Pure;

   Nul                                      : constant := 16#00_0000#;

   Backspace                                : constant := 16#00_0008#;
   Character_Tabulation                     : constant := 16#00_0009#;
   Line_Feed                                : constant := 16#00_000A#;
   Line_Tabulation                          : constant := 16#00_000B#;
   Form_Feed                                : constant := 16#00_000C#;
   Carriage_Return                          : constant := 16#00_000D#;

   Space                                    : constant := 16#00_0020#;  --  ' '

   Quotation_Mark                           : constant := 16#00_0022#;  --  '"'
   Number_Sign                              : constant := 16#00_0023#;  --  '#'
   Dollar_Sign                              : constant := 16#00_0024#;  --  '$'

   Apostrophe                               : constant := 16#00_0027#;  --  '''

   Asterisk                                 : constant := 16#00_002A#;  --  '*'
   Plus_Sign                                : constant := 16#00_002B#;  --  '+'
   Comma                                    : constant := 16#00_002C#;  --  ','
   Hyphen_Minus                             : constant := 16#00_002D#;  --  '-'
   Full_Stop                                : constant := 16#00_002E#;  --  '.'
   Solidus                                  : constant := 16#00_002F#;  --  '/'

   Digit_Zero                               : constant := 16#00_0030#;  --  '0'
   Digit_One                                : constant := 16#00_0031#;  --  '1'

   Digit_Nine                               : constant := 16#00_0039#;  --  '9'
   Colon                                    : constant := 16#00_003A#;  --  ':'

   Latin_Capital_Letter_A                   : constant := 16#00_0041#;  --  'A'

   Latin_Capital_Letter_E                   : constant := 16#00_0045#;  --  'E'
   Latin_Capital_Letter_F                   : constant := 16#00_0046#;  --  'F'

   Latin_Capital_Letter_I                   : constant := 16#00_0049#;  --  'I'

   Latin_Capital_Letter_N                   : constant := 16#00_004E#;  --  'N'

   Latin_Capital_Letter_X                   : constant := 16#00_0058#;  --  'X'

   Left_Square_Bracket                      : constant := 16#00_005B#;  --  '['
   Reverse_Solidus                          : constant := 16#00_005C#;  --  '\'
   Right_Square_Bracket                     : constant := 16#00_005D#;  --  ']'

   Low_Line                                 : constant := 16#00_005F#;  --  '_'

   Latin_Small_Letter_A                     : constant := 16#00_0061#;  --  'a'
   Latin_Small_Letter_B                     : constant := 16#00_0062#;  --  'b'

   Latin_Small_Letter_E                     : constant := 16#00_0065#;  --  'e'
   Latin_Small_Letter_F                     : constant := 16#00_0066#;  --  'f'

   Latin_Small_Letter_I                     : constant := 16#00_0069#;  --  'i'

   Latin_Small_Letter_L                     : constant := 16#00_006C#;  --  'l'

   Latin_Small_Letter_N                     : constant := 16#00_006E#;  --  'n'

   Latin_Small_Letter_R                     : constant := 16#00_0072#;  --  'r'
   Latin_Small_Letter_S                     : constant := 16#00_0073#;  --  's'
   Latin_Small_Letter_T                     : constant := 16#00_0074#;  --  't'
   Latin_Small_Letter_U                     : constant := 16#00_0075#;  --  'u'
   Latin_Small_Letter_V                     : constant := 16#00_0076#;  --  'v'

   Latin_Small_Letter_X                     : constant := 16#00_0078#;  --  'x'
   Latin_Small_Letter_Y                     : constant := 16#00_0079#;  --  'y'

   Left_Curly_Bracket                       : constant := 16#00_007B#;  --  '{'

   Right_Curly_Bracket                      : constant := 16#00_007D#;  --  '}'

   Next_Line                                : constant := 16#00_0085#;

   No_Break_Space                           : constant := 16#00_00A0#;

   Combining_Enclosing_Keycap               : constant := 16#00_20E3#;

   Zero_Width_Non_Joiner                    : constant := 16#00_200C#;
   Zero_Width_Joiner                        : constant := 16#00_200D#;

   Line_Separator                           : constant := 16#00_2028#;
   Paragraph_Separator                      : constant := 16#00_2029#;

   Variation_Selector_16                    : constant := 16#00_FE0F#;

   Zero_Width_No_Break_Space                : constant := 16#00_FEFF#;

end VSS.Implementation.Character_Codes;
