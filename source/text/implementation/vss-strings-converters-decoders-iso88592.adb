--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Text_Handlers;

package body VSS.Strings.Converters.Decoders.ISO88592 is

   Mapping : constant
     array (Ada.Streams.Stream_Element range 16#A0# .. 16#FF#)
       of VSS.Unicode.Code_Point :=
         [16#A0# => 16#00A0#,  --  NO-BREAK SPACE
          16#A1# => 16#0104#,  --  LATIN CAPITAL LETTER A WITH OGONEK
          16#A2# => 16#02D8#,  --  BREVE
          16#A3# => 16#0141#,  --  LATIN CAPITAL LETTER L WITH STROKE
          16#A4# => 16#00A4#,  --  CURRENCY SIGN
          16#A5# => 16#013D#,  --  LATIN CAPITAL LETTER L WITH CARON
          16#A6# => 16#015A#,  --  LATIN CAPITAL LETTER S WITH ACUTE
          16#A7# => 16#00A7#,  --  SECTION SIGN
          16#A8# => 16#00A8#,  --  DIAERESIS
          16#A9# => 16#0160#,  --  LATIN CAPITAL LETTER S WITH CARON
          16#AA# => 16#015E#,  --  LATIN CAPITAL LETTER S WITH CEDILLA
          16#AB# => 16#0164#,  --  LATIN CAPITAL LETTER T WITH CARON
          16#AC# => 16#0179#,  --  LATIN CAPITAL LETTER Z WITH ACUTE
          16#AD# => 16#00AD#,  --  SOFT HYPHEN
          16#AE# => 16#017D#,  --  LATIN CAPITAL LETTER Z WITH CARON
          16#AF# => 16#017B#,  --  LATIN CAPITAL LETTER Z WITH DOT ABOVE
          16#B0# => 16#00B0#,  --  DEGREE SIGN
          16#B1# => 16#0105#,  --  LATIN SMALL LETTER A WITH OGONEK
          16#B2# => 16#02DB#,  --  OGONEK
          16#B3# => 16#0142#,  --  LATIN SMALL LETTER L WITH STROKE
          16#B4# => 16#00B4#,  --  ACUTE ACCENT
          16#B5# => 16#013E#,  --  LATIN SMALL LETTER L WITH CARON
          16#B6# => 16#015B#,  --  LATIN SMALL LETTER S WITH ACUTE
          16#B7# => 16#02C7#,  --  CARON
          16#B8# => 16#00B8#,  --  CEDILLA
          16#B9# => 16#0161#,  --  LATIN SMALL LETTER S WITH CARON
          16#BA# => 16#015F#,  --  LATIN SMALL LETTER S WITH CEDILLA
          16#BB# => 16#0165#,  --  LATIN SMALL LETTER T WITH CARON
          16#BC# => 16#017A#,  --  LATIN SMALL LETTER Z WITH ACUTE
          16#BD# => 16#02DD#,  --  DOUBLE ACUTE ACCENT
          16#BE# => 16#017E#,  --  LATIN SMALL LETTER Z WITH CARON
          16#BF# => 16#017C#,  --  LATIN SMALL LETTER Z WITH DOT ABOVE
          16#C0# => 16#0154#,  --  LATIN CAPITAL LETTER R WITH ACUTE
          16#C1# => 16#00C1#,  --  LATIN CAPITAL LETTER A WITH ACUTE
          16#C2# => 16#00C2#,  --  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
          16#C3# => 16#0102#,  --  LATIN CAPITAL LETTER A WITH BREVE
          16#C4# => 16#00C4#,  --  LATIN CAPITAL LETTER A WITH DIAERESIS
          16#C5# => 16#0139#,  --  LATIN CAPITAL LETTER L WITH ACUTE
          16#C6# => 16#0106#,  --  LATIN CAPITAL LETTER C WITH ACUTE
          16#C7# => 16#00C7#,  --  LATIN CAPITAL LETTER C WITH CEDILLA
          16#C8# => 16#010C#,  --  LATIN CAPITAL LETTER C WITH CARON
          16#C9# => 16#00C9#,  --  LATIN CAPITAL LETTER E WITH ACUTE
          16#CA# => 16#0118#,  --  LATIN CAPITAL LETTER E WITH OGONEK
          16#CB# => 16#00CB#,  --  LATIN CAPITAL LETTER E WITH DIAERESIS
          16#CC# => 16#011A#,  --  LATIN CAPITAL LETTER E WITH CARON
          16#CD# => 16#00CD#,  --  LATIN CAPITAL LETTER I WITH ACUTE
          16#CE# => 16#00CE#,  --  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
          16#CF# => 16#010E#,  --  LATIN CAPITAL LETTER D WITH CARON
          16#D0# => 16#0110#,  --  LATIN CAPITAL LETTER D WITH STROKE
          16#D1# => 16#0143#,  --  LATIN CAPITAL LETTER N WITH ACUTE
          16#D2# => 16#0147#,  --  LATIN CAPITAL LETTER N WITH CARON
          16#D3# => 16#00D3#,  --  LATIN CAPITAL LETTER O WITH ACUTE
          16#D4# => 16#00D4#,  --  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
          16#D5# => 16#0150#,  --  LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
          16#D6# => 16#00D6#,  --  LATIN CAPITAL LETTER O WITH DIAERESIS
          16#D7# => 16#00D7#,  --  MULTIPLICATION SIGN
          16#D8# => 16#0158#,  --  LATIN CAPITAL LETTER R WITH CARON
          16#D9# => 16#016E#,  --  LATIN CAPITAL LETTER U WITH RING ABOVE
          16#DA# => 16#00DA#,  --  LATIN CAPITAL LETTER U WITH ACUTE
          16#DB# => 16#0170#,  --  LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
          16#DC# => 16#00DC#,  --  LATIN CAPITAL LETTER U WITH DIAERESIS
          16#DD# => 16#00DD#,  --  LATIN CAPITAL LETTER Y WITH ACUTE
          16#DE# => 16#0162#,  --  LATIN CAPITAL LETTER T WITH CEDILLA
          16#DF# => 16#00DF#,  --  LATIN SMALL LETTER SHARP S
          16#E0# => 16#0155#,  --  LATIN SMALL LETTER R WITH ACUTE
          16#E1# => 16#00E1#,  --  LATIN SMALL LETTER A WITH ACUTE
          16#E2# => 16#00E2#,  --  LATIN SMALL LETTER A WITH CIRCUMFLEX
          16#E3# => 16#0103#,  --  LATIN SMALL LETTER A WITH BREVE
          16#E4# => 16#00E4#,  --  LATIN SMALL LETTER A WITH DIAERESIS
          16#E5# => 16#013A#,  --  LATIN SMALL LETTER L WITH ACUTE
          16#E6# => 16#0107#,  --  LATIN SMALL LETTER C WITH ACUTE
          16#E7# => 16#00E7#,  --  LATIN SMALL LETTER C WITH CEDILLA
          16#E8# => 16#010D#,  --  LATIN SMALL LETTER C WITH CARON
          16#E9# => 16#00E9#,  --  LATIN SMALL LETTER E WITH ACUTE
          16#EA# => 16#0119#,  --  LATIN SMALL LETTER E WITH OGONEK
          16#EB# => 16#00EB#,  --  LATIN SMALL LETTER E WITH DIAERESIS
          16#EC# => 16#011B#,  --  LATIN SMALL LETTER E WITH CARON
          16#ED# => 16#00ED#,  --  LATIN SMALL LETTER I WITH ACUTE
          16#EE# => 16#00EE#,  --  LATIN SMALL LETTER I WITH CIRCUMFLEX
          16#EF# => 16#010F#,  --  LATIN SMALL LETTER D WITH CARON
          16#F0# => 16#0111#,  --  LATIN SMALL LETTER D WITH STROKE
          16#F1# => 16#0144#,  --  LATIN SMALL LETTER N WITH ACUTE
          16#F2# => 16#0148#,  --  LATIN SMALL LETTER N WITH CARON
          16#F3# => 16#00F3#,  --  LATIN SMALL LETTER O WITH ACUTE
          16#F4# => 16#00F4#,  --  LATIN SMALL LETTER O WITH CIRCUMFLEX
          16#F5# => 16#0151#,  --  LATIN SMALL LETTER O WITH DOUBLE ACUTE
          16#F6# => 16#00F6#,  --  LATIN SMALL LETTER O WITH DIAERESIS
          16#F7# => 16#00F7#,  --  DIVISION SIGN
          16#F8# => 16#0159#,  --  LATIN SMALL LETTER R WITH CARON
          16#F9# => 16#016F#,  --  LATIN SMALL LETTER U WITH RING ABOVE
          16#FA# => 16#00FA#,  --  LATIN SMALL LETTER U WITH ACUTE
          16#FB# => 16#0171#,  --  LATIN SMALL LETTER U WITH DOUBLE ACUTE
          16#FC# => 16#00FC#,  --  LATIN SMALL LETTER U WITH DIAERESIS
          16#FD# => 16#00FD#,  --  LATIN SMALL LETTER Y WITH ACUTE
          16#FE# => 16#0163#,  --  LATIN SMALL LETTER T WITH CEDILLA
          16#FF# => 16#02D9#]; --  DOT ABOVE

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out ISO88592_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (End_Of_Data);

      use type Ada.Streams.Stream_Element_Offset;

      Index   : Ada.Streams.Stream_Element_Offset := Source'First;
      Byte    : Ada.Streams.Stream_Element;
      Offset  : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);
      Handler : VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
      loop
         exit when Index > Source'Last;

         Byte := Source (Index);

         case Byte is
            when Mapping'Range =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, Mapping (Byte), Offset);

            when others =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, VSS.Unicode.Code_Point (Byte), Offset);
         end case;

         Index := Index + 1;
      end loop;
   end Decode;

   -------------
   -- Factory --
   -------------

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access
   is
      pragma Unreferenced (Flags);

   begin
      return new ISO88592_Decoder;
   end Factory;

end VSS.Strings.Converters.Decoders.ISO88592;
