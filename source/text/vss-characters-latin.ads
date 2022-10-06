--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Constants for named characters.

package VSS.Characters.Latin is

   pragma Preelaborate;

   Nul       : constant Virtual_Character := Virtual_Character'Val (16#0000#);

   Backspace : constant Virtual_Character := Virtual_Character'Val (16#0008#);
   Character_Tabulation : constant Virtual_Character :=
     Virtual_Character'Val (16#0009#);
   Line_Feed : constant Virtual_Character := Virtual_Character'Val (16#000A#);

   Form_Feed : constant Virtual_Character := Virtual_Character'Val (16#000C#);
   Carriage_Return : constant Virtual_Character :=
     Virtual_Character'Val (16#000D#);

   Space     : constant Virtual_Character := Virtual_Character'Val (16#0020#);

   Exclamation_Mark       : constant Virtual_Character :=
     Virtual_Character'Val (16#0021#);
   Quotation_Mark         : constant Virtual_Character :=
     Virtual_Character'Val (16#0022#);
   Number_Sign            : constant Virtual_Character :=
     Virtual_Character'Val (16#0023#);
   Dollar_Sign            : constant Virtual_Character :=
     Virtual_Character'Val (16#0024#);
   Percent_Sign           : constant Virtual_Character :=
     Virtual_Character'Val (16#0025#);
   Ampersand              : constant Virtual_Character :=
     Virtual_Character'Val (16#0026#);
   Apostrophe             : constant Virtual_Character :=
     Virtual_Character'Val (16#0027#);

   Plus_Sign              : constant Virtual_Character :=
     Virtual_Character'Val (16#002B#);

   Hyphen_Minus           : constant Virtual_Character :=
     Virtual_Character'Val (16#002D#);

   Digit_Zero             : constant Virtual_Character :=
     Virtual_Character'Val (16#0030#);

   Digit_Nine             : constant Virtual_Character :=
     Virtual_Character'Val (16#0039#);

   Less_Than_Sign         : constant Virtual_Character :=
     Virtual_Character'Val (16#003C#);
   Equals_Sign            : constant Virtual_Character :=
     Virtual_Character'Val (16#003D#);
   Greater_Than_Sign      : constant Virtual_Character :=
     Virtual_Character'Val (16#003E#);

   Latin_Capital_Letter_A : constant Virtual_Character :=
     Virtual_Character'Val (16#0041#);

   Latin_Capital_Letter_Z : constant Virtual_Character :=
     Virtual_Character'Val (16#005A#);

   Circumflex_Accent      : constant Virtual_Character :=
     Virtual_Character'Val (16#005E#);
   Low_Line               : constant Virtual_Character :=
     Virtual_Character'Val (16#005F#);
   Grave_Accent           : constant Virtual_Character :=
     Virtual_Character'Val (16#0060#);
   Latin_Small_Letter_A   : constant Virtual_Character :=
     Virtual_Character'Val (16#0061#);

   Latin_Small_Letter_Z   : constant Virtual_Character :=
     Virtual_Character'Val (16#007A#);
   Left_Curly_Bracket     : constant Virtual_Character :=
     Virtual_Character'Val (16#007B#);

   Right_Curly_Bracket    : constant Virtual_Character :=
     Virtual_Character'Val (16#007D#);
   Tilde                  : constant Virtual_Character :=
     Virtual_Character'Val (16#007E#);

end VSS.Characters.Latin;
