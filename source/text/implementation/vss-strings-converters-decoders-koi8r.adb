--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Text_Handlers;

package body VSS.Strings.Converters.Decoders.KOI8R is

   pragma Style_Checks ("M131");

   Mapping : constant
     array (Ada.Streams.Stream_Element range 16#80# .. 16#FF#)
       of VSS.Unicode.Code_Point :=
       [16#80# => 16#2500#,   --  BOX DRAWINGS LIGHT HORIZONTAL
        16#81# => 16#2502#,   --  BOX DRAWINGS LIGHT VERTICAL
        16#82# => 16#250C#,   --  BOX DRAWINGS LIGHT DOWN AND RIGHT
        16#83# => 16#2510#,   --  BOX DRAWINGS LIGHT DOWN AND LEFT
        16#84# => 16#2514#,   --  BOX DRAWINGS LIGHT UP AND RIGHT
        16#85# => 16#2518#,   --  BOX DRAWINGS LIGHT UP AND LEFT
        16#86# => 16#251C#,   --  BOX DRAWINGS LIGHT VERTICAL AND RIGHT
        16#87# => 16#2524#,   --  BOX DRAWINGS LIGHT VERTICAL AND LEFT
        16#88# => 16#252C#,   --  BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
        16#89# => 16#2534#,   --  BOX DRAWINGS LIGHT UP AND HORIZONTAL
        16#8A# => 16#253C#,   --  BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
        16#8B# => 16#2580#,   --  UPPER HALF BLOCK
        16#8C# => 16#2584#,   --  LOWER HALF BLOCK
        16#8D# => 16#2588#,   --  FULL BLOCK
        16#8E# => 16#258C#,   --  LEFT HALF BLOCK
        16#8F# => 16#2590#,   --  RIGHT HALF BLOCK
        16#90# => 16#2591#,   --  LIGHT SHADE
        16#91# => 16#2592#,   --  MEDIUM SHADE
        16#92# => 16#2593#,   --  DARK SHADE
        16#93# => 16#2320#,   --  TOP HALF INTEGRAL
        16#94# => 16#25A0#,   --  BLACK SQUARE
        16#95# => 16#2219#,   --  BULLET OPERATOR
        16#96# => 16#221A#,   --  SQUARE ROOT
        16#97# => 16#2248#,   --  ALMOST EQUAL TO
        16#98# => 16#2264#,   --  LESS-THAN OR EQUAL TO
        16#99# => 16#2265#,   --  GREATER-THAN OR EQUAL TO
        16#9A# => 16#00A0#,   --  NO-BREAK SPACE
        16#9B# => 16#2321#,   --  BOTTOM HALF INTEGRAL
        16#9C# => 16#00B0#,   --  DEGREE SIGN
        16#9D# => 16#00B2#,   --  SUPERSCRIPT TWO
        16#9E# => 16#00B7#,   --  MIDDLE DOT
        16#9F# => 16#00F7#,   --  DIVISION SIGN
        16#A0# => 16#2550#,   --  BOX DRAWINGS DOUBLE HORIZONTAL
        16#A1# => 16#2551#,   --  BOX DRAWINGS DOUBLE VERTICAL
        16#A2# => 16#2552#,   --  BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
        16#A3# => 16#0451#,   --  CYRILLIC SMALL LETTER IO
        16#A4# => 16#2553#,   --  BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
        16#A5# => 16#2554#,   --  BOX DRAWINGS DOUBLE DOWN AND RIGHT
        16#A6# => 16#2555#,   --  BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
        16#A7# => 16#2556#,   --  BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
        16#A8# => 16#2557#,   --  BOX DRAWINGS DOUBLE DOWN AND LEFT
        16#A9# => 16#2558#,   --  BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
        16#AA# => 16#2559#,   --  BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
        16#AB# => 16#255A#,   --  BOX DRAWINGS DOUBLE UP AND RIGHT
        16#AC# => 16#255B#,   --  BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
        16#AD# => 16#255C#,   --  BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
        16#AE# => 16#255D#,   --  BOX DRAWINGS DOUBLE UP AND LEFT
        16#AF# => 16#255E#,   --  BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
        16#B0# => 16#255F#,   --  BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
        16#B1# => 16#2560#,   --  BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
        16#B2# => 16#2561#,   --  BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
        16#B3# => 16#0401#,   --  CYRILLIC CAPITAL LETTER IO
        16#B4# => 16#2562#,   --  BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
        16#B5# => 16#2563#,   --  BOX DRAWINGS DOUBLE VERTICAL AND LEFT
        16#B6# => 16#2564#,   --  BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
        16#B7# => 16#2565#,   --  BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
        16#B8# => 16#2566#,   --  BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
        16#B9# => 16#2567#,   --  BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
        16#BA# => 16#2568#,   --  BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
        16#BB# => 16#2569#,   --  BOX DRAWINGS DOUBLE UP AND HORIZONTAL
        16#BC# => 16#256A#,   --  BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
        16#BD# => 16#256B#,   --  BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
        16#BE# => 16#256C#,   --  BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
        16#BF# => 16#00A9#,   --  COPYRIGHT SIGN
        16#C0# => 16#044E#,   --  CYRILLIC SMALL LETTER YU
        16#C1# => 16#0430#,   --  CYRILLIC SMALL LETTER A
        16#C2# => 16#0431#,   --  CYRILLIC SMALL LETTER BE
        16#C3# => 16#0446#,   --  CYRILLIC SMALL LETTER TSE
        16#C4# => 16#0434#,   --  CYRILLIC SMALL LETTER DE
        16#C5# => 16#0435#,   --  CYRILLIC SMALL LETTER IE
        16#C6# => 16#0444#,   --  CYRILLIC SMALL LETTER EF
        16#C7# => 16#0433#,   --  CYRILLIC SMALL LETTER GHE
        16#C8# => 16#0445#,   --  CYRILLIC SMALL LETTER HA
        16#C9# => 16#0438#,   --  CYRILLIC SMALL LETTER I
        16#CA# => 16#0439#,   --  CYRILLIC SMALL LETTER SHORT I
        16#CB# => 16#043A#,   --  CYRILLIC SMALL LETTER KA
        16#CC# => 16#043B#,   --  CYRILLIC SMALL LETTER EL
        16#CD# => 16#043C#,   --  CYRILLIC SMALL LETTER EM
        16#CE# => 16#043D#,   --  CYRILLIC SMALL LETTER EN
        16#CF# => 16#043E#,   --  CYRILLIC SMALL LETTER O
        16#D0# => 16#043F#,   --  CYRILLIC SMALL LETTER PE
        16#D1# => 16#044F#,   --  CYRILLIC SMALL LETTER YA
        16#D2# => 16#0440#,   --  CYRILLIC SMALL LETTER ER
        16#D3# => 16#0441#,   --  CYRILLIC SMALL LETTER ES
        16#D4# => 16#0442#,   --  CYRILLIC SMALL LETTER TE
        16#D5# => 16#0443#,   --  CYRILLIC SMALL LETTER U
        16#D6# => 16#0436#,   --  CYRILLIC SMALL LETTER ZHE
        16#D7# => 16#0432#,   --  CYRILLIC SMALL LETTER VE
        16#D8# => 16#044C#,   --  CYRILLIC SMALL LETTER SOFT SIGN
        16#D9# => 16#044B#,   --  CYRILLIC SMALL LETTER YERU
        16#DA# => 16#0437#,   --  CYRILLIC SMALL LETTER ZE
        16#DB# => 16#0448#,   --  CYRILLIC SMALL LETTER SHA
        16#DC# => 16#044D#,   --  CYRILLIC SMALL LETTER E
        16#DD# => 16#0449#,   --  CYRILLIC SMALL LETTER SHCHA
        16#DE# => 16#0447#,   --  CYRILLIC SMALL LETTER CHE
        16#DF# => 16#044A#,   --  CYRILLIC SMALL LETTER HARD SIGN
        16#E0# => 16#042E#,   --  CYRILLIC CAPITAL LETTER YU
        16#E1# => 16#0410#,   --  CYRILLIC CAPITAL LETTER A
        16#E2# => 16#0411#,   --  CYRILLIC CAPITAL LETTER BE
        16#E3# => 16#0426#,   --  CYRILLIC CAPITAL LETTER TSE
        16#E4# => 16#0414#,   --  CYRILLIC CAPITAL LETTER DE
        16#E5# => 16#0415#,   --  CYRILLIC CAPITAL LETTER IE
        16#E6# => 16#0424#,   --  CYRILLIC CAPITAL LETTER EF
        16#E7# => 16#0413#,   --  CYRILLIC CAPITAL LETTER GHE
        16#E8# => 16#0425#,   --  CYRILLIC CAPITAL LETTER HA
        16#E9# => 16#0418#,   --  CYRILLIC CAPITAL LETTER I
        16#EA# => 16#0419#,   --  CYRILLIC CAPITAL LETTER SHORT I
        16#EB# => 16#041A#,   --  CYRILLIC CAPITAL LETTER KA
        16#EC# => 16#041B#,   --  CYRILLIC CAPITAL LETTER EL
        16#ED# => 16#041C#,   --  CYRILLIC CAPITAL LETTER EM
        16#EE# => 16#041D#,   --  CYRILLIC CAPITAL LETTER EN
        16#EF# => 16#041E#,   --  CYRILLIC CAPITAL LETTER O
        16#F0# => 16#041F#,   --  CYRILLIC CAPITAL LETTER PE
        16#F1# => 16#042F#,   --  CYRILLIC CAPITAL LETTER YA
        16#F2# => 16#0420#,   --  CYRILLIC CAPITAL LETTER ER
        16#F3# => 16#0421#,   --  CYRILLIC CAPITAL LETTER ES
        16#F4# => 16#0422#,   --  CYRILLIC CAPITAL LETTER TE
        16#F5# => 16#0423#,   --  CYRILLIC CAPITAL LETTER U
        16#F6# => 16#0416#,   --  CYRILLIC CAPITAL LETTER ZHE
        16#F7# => 16#0412#,   --  CYRILLIC CAPITAL LETTER VE
        16#F8# => 16#042C#,   --  CYRILLIC CAPITAL LETTER SOFT SIGN
        16#F9# => 16#042B#,   --  CYRILLIC CAPITAL LETTER YERU
        16#FA# => 16#0417#,   --  CYRILLIC CAPITAL LETTER ZE
        16#FB# => 16#0428#,   --  CYRILLIC CAPITAL LETTER SHA
        16#FC# => 16#042D#,   --  CYRILLIC CAPITAL LETTER E
        16#FD# => 16#0429#,   --  CYRILLIC CAPITAL LETTER SHCHA
        16#FE# => 16#0427#,   --  CYRILLIC CAPITAL LETTER CHE
        16#FF# => 16#042A#];  --  CYRILLIC CAPITAL LETTER HARD SIGN

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out KOI8R_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (End_Of_Data);

      use type Ada.Streams.Stream_Element_Offset;

      Index  : Ada.Streams.Stream_Element_Offset := Source'First;
      Byte   : Ada.Streams.Stream_Element;
      Offset : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      loop
         exit when Index > Source'Last;

         Byte := Source (Index);

         case Byte is
            when Mapping'Range =>
               VSS.Implementation.Strings.Variable_Handler (Target).Append
                 (Target, Mapping (Byte), Offset);

            when others =>
               VSS.Implementation.Strings.Variable_Handler (Target).Append
                 (Target, VSS.Unicode.Code_Point (Byte), Offset);
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
      return new KOI8R_Decoder;
   end Factory;

end VSS.Strings.Converters.Decoders.KOI8R;
