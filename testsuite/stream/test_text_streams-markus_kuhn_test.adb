--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Characters;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings;

separate (Test_Text_Streams)

procedure Markus_Kuhn_Test is

   type Correct_Testcase is record
      Data : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Text : VSS.Strings.Virtual_String;
   end record;

   type Malformed_Testcase is record
      Data  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Error : VSS.Strings.Virtual_String;
   end record;

   Case_1 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1  => 16#CE#,
          2  => 16#BA#,
          3  => 16#E1#,
          4  => 16#BD#,
          5  => 16#B9#,
          6  => 16#CF#,
          7  => 16#83#,
          8  => 16#CE#,
          9  => 16#BC#,
          10 => 16#CE#,
          11 => 16#B5#)),
      VSS.Strings.To_Virtual_String ("κόσμε"));

   Case_2_1_1 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#00#)),
      VSS.Strings.To_Virtual_String ((1 => Wide_Wide_Character'Val (16#00#))));

   Case_2_1_2 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#C2#,
          2 => 16#80#)),
      VSS.Strings.To_Virtual_String ((1 => Wide_Wide_Character'Val (16#80#))));

   Case_2_1_3 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#E0#,
          2 => 16#A0#,
          3 => 16#80#)),
      VSS.Strings.To_Virtual_String ("ࠀ"));

   Case_2_1_4 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F0#,
          2 => 16#90#,
          3 => 16#80#,
          4 => 16#80#)),
      VSS.Strings.To_Virtual_String ("𐀀"));

   --  Case_2_1_5 and Case_2_1_6 are outside of Unicode code point range and
   --  are not supported

   Case_2_2_1 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#7F#)),
      VSS.Strings.To_Virtual_String ((1 => Wide_Wide_Character'Val (16#7F#))));

   Case_2_2_2 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#DF#,
          2 => 16#BF#)),
      VSS.Strings.To_Virtual_String ("߿"));

   Case_2_2_3 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#EF#,
          2 => 16#BF#,
          3 => 16#BF#)),
      VSS.Strings.To_Virtual_String
        ((1 => Wide_Wide_Character'Val (16#FFFF#))));

   --  Case_2_2_4, Case_2_2_5 and Case_2_2_6 are outside of Unicode code point
   --  range and are not supported

   Case_2_3_1 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#9F#,
          3 => 16#BF#)),
      VSS.Strings.To_Virtual_String ("퟿"));

   Case_2_3_2 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#EE#,
          2 => 16#80#,
          3 => 16#80#)),
      VSS.Strings.To_Virtual_String
        ((1 => Wide_Wide_Character'Val (16#E000#))));

   Case_2_3_3 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#EF#,
          2 => 16#BF#,
          3 => 16#BD#)),
      VSS.Strings.To_Virtual_String ("�"));

   Case_2_3_4 : constant Correct_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F4#,
          2 => 16#8F#,
          3 => 16#BF#,
          4 => 16#BF#)),
      VSS.Strings.To_Virtual_String
        ((1 => Wide_Wide_Character'Val (16#10_FFFF#))));

   --  Case_2_3_5 uses code point outside of Unicode range

   Error_Incomplete_2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("incomplete two code unit sequence");

   Error_Incomplete_3 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("incomplete three code unit sequence");

   Error_Incomplete_4 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("incomplete four code unit sequence");

   Error_Invalid_1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("invalid UTF-8 sequence (wrong start code unit)");

   Error_Invalid_2_3 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("invalid UTF-8 sequence (wrong second code unit of"
        & " three code units sequence)");

   Error_Invalid_2_4 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("invalid UTF-8 sequence (wrong second code unit of"
        & " four code units sequence)");

   Case_3_1_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#80#)),
      Error_Invalid_1);
   Case_3_1_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#BF#)),
      Error_Invalid_1);
   Case_3_1_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#80#,
          2 => 16#BF#)),
      Error_Invalid_1);
   Case_3_1_4 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#80#,
          2 => 16#BF#,
          3 => 16#80#)),
      Error_Invalid_1);
   Case_3_1_5 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#80#,
          2 => 16#BF#,
          3 => 16#80#,
          4 => 16#BF#)),
      Error_Invalid_1);
   Case_3_1_6 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#80#,
          2 => 16#BF#,
          3 => 16#80#,
          4 => 16#BF#,
          5 => 16#80#)),
      Error_Invalid_1);
   Case_3_1_7 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#80#,
          2 => 16#BF#,
          3 => 16#80#,
          4 => 16#BF#,
          5 => 16#80#,
          6 => 16#BF#)),
      Error_Invalid_1);
   Case_3_1_8 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#80#,
          2 => 16#BF#,
          3 => 16#80#,
          4 => 16#BF#,
          5 => 16#80#,
          6 => 16#BF#,
          7 => 16#80#)),
      Error_Invalid_1);
   Case_3_1_9 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1  => 16#80#,
          2  => 16#81#,
          3  => 16#82#,
          4  => 16#83#,
          5  => 16#84#,
          6  => 16#85#,
          7  => 16#86#,
          8  => 16#87#,
          9  => 16#88#,
          10 => 16#89#,
          11 => 16#8A#,
          12 => 16#8B#,
          13 => 16#8C#,
          14 => 16#8D#,
          15 => 16#8E#,
          16 => 16#8F#,

          17 => 16#90#,
          18 => 16#91#,
          19 => 16#92#,
          20 => 16#93#,
          21 => 16#94#,
          22 => 16#95#,
          23 => 16#96#,
          24 => 16#97#,
          25 => 16#98#,
          26 => 16#99#,
          27 => 16#9A#,
          28 => 16#9B#,
          29 => 16#9C#,
          30 => 16#9D#,
          31 => 16#9E#,
          32 => 16#9F#,

          33 => 16#A0#,
          34 => 16#A1#,
          35 => 16#A2#,
          36 => 16#A3#,
          37 => 16#A4#,
          38 => 16#A5#,
          39 => 16#A6#,
          40 => 16#A7#,
          41 => 16#A8#,
          42 => 16#A9#,
          43 => 16#AA#,
          44 => 16#AB#,
          45 => 16#AC#,
          46 => 16#AD#,
          47 => 16#AE#,
          48 => 16#AF#,

          49 => 16#B0#,
          50 => 16#B1#,
          51 => 16#B2#,
          52 => 16#B3#,
          53 => 16#B4#,
          54 => 16#B5#,
          55 => 16#B6#,
          56 => 16#B7#,
          57 => 16#B8#,
          58 => 16#B9#,
          59 => 16#BA#,
          60 => 16#BB#,
          61 => 16#BC#,
          62 => 16#BD#,
          63 => 16#BE#,
          64 => 16#BF#)),
      Error_Invalid_1);

   Case_3_2_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1  => 16#C0#,
          2  => 16#20#,
          3  => 16#C1#,
          4  => 16#20#,
          5  => 16#C2#,
          6  => 16#20#,
          7  => 16#C3#,
          8  => 16#20#,
          9  => 16#C4#,
          10 => 16#20#,
          11 => 16#C5#,
          12 => 16#20#,
          13 => 16#C6#,
          14 => 16#20#,
          15 => 16#C7#,
          16 => 16#20#,
          17 => 16#C8#,
          18 => 16#20#,
          19 => 16#C9#,
          20 => 16#20#,
          21 => 16#CA#,
          22 => 16#20#,
          23 => 16#CB#,
          24 => 16#20#,
          25 => 16#CC#,
          26 => 16#20#,
          27 => 16#CD#,
          28 => 16#20#,
          29 => 16#CE#,
          30 => 16#20#,
          31 => 16#CF#,
          32 => 16#20#,

          33 => 16#D0#,
          34 => 16#20#,
          35 => 16#D1#,
          36 => 16#20#,
          37 => 16#D2#,
          38 => 16#20#,
          39 => 16#D3#,
          40 => 16#20#,
          41 => 16#D4#,
          42 => 16#20#,
          43 => 16#D5#,
          44 => 16#20#,
          45 => 16#D6#,
          46 => 16#20#,
          47 => 16#D7#,
          48 => 16#20#,
          49 => 16#D8#,
          50 => 16#20#,
          51 => 16#D9#,
          52 => 16#20#,
          53 => 16#DA#,
          54 => 16#20#,
          55 => 16#DB#,
          56 => 16#20#,
          57 => 16#DC#,
          58 => 16#20#,
          59 => 16#DD#,
          60 => 16#20#,
          61 => 16#DE#,
          62 => 16#20#,
          63 => 16#DF#,
          64 => 16#20#)),
      Error_Invalid_1);
   --  XXX Need to be splitted into 32 separate testcases?

   Case_3_2_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1  => 16#E0#,
          2  => 16#20#,
          3  => 16#E1#,
          4  => 16#20#,
          5  => 16#E2#,
          6  => 16#20#,
          7  => 16#E3#,
          8  => 16#20#,
          9  => 16#E4#,
          10 => 16#20#,
          11 => 16#E5#,
          12 => 16#20#,
          13 => 16#E6#,
          14 => 16#20#,
          15 => 16#E7#,
          16 => 16#20#,
          17 => 16#E8#,
          18 => 16#20#,
          19 => 16#E9#,
          20 => 16#20#,
          21 => 16#EA#,
          22 => 16#20#,
          23 => 16#EB#,
          24 => 16#20#,
          25 => 16#EC#,
          26 => 16#20#,
          27 => 16#ED#,
          28 => 16#20#,
          29 => 16#EE#,
          30 => 16#20#,
          31 => 16#EF#,
          32 => 16#20#)),
      Error_Invalid_2_3);
   --  XXX Need to be splitted into 16 separate testcases?

   Case_3_2_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1  => 16#F0#,
          2  => 16#20#,
          3  => 16#F1#,
          4  => 16#20#,
          5  => 16#F2#,
          6  => 16#20#,
          7  => 16#F3#,
          8  => 16#20#,
          9  => 16#F4#,
          10 => 16#20#,
          11 => 16#F5#,
          12 => 16#20#,
          13 => 16#F6#,
          14 => 16#20#,
          15 => 16#F7#)),
      Error_Invalid_2_4);
   --  XXX Need to be splitted into 16 separate testcases?

   --  Case_3_2_4 and Case_3_2_5 are not supported

   Case_3_3_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#C0#)),
      Error_Invalid_1);

   Case_3_3_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#E0#,
          2 => 16#80#)),
      Error_Incomplete_3);

   Case_3_3_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F0#,
          2 => 16#80#,
          3 => 16#80#)),
      Error_Incomplete_4);

   --  Case_3_3_4 and Case_3_3_5 are not applicable: they cover unsupported 5
   --  and 6 code unit sequences

   Case_3_3_6 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#DF#)),
      Error_Incomplete_2);

   Case_3_3_7 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#EF#,
          2 => 16#BF#)),
      Error_Incomplete_3);

   Case_3_3_8 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F7#,
          2 => 16#BF#,
          3 => 16#BF#)),
      Error_Invalid_1);
   --  Tested code sequence represents code point outside of Unicode range.

   --  Case_3_3_9 and Case_3_3_10 are not applicable: they cover unsupported
   --  5 and 6 code unit sequences.

   Case_3_4 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1  => 16#C0#,

          2  => 16#E0#,
          3  => 16#80#,

          4  => 16#F0#,
          5  => 16#80#,
          6  => 16#80#,

          7  => 16#F8#,
          8  => 16#80#,
          9  => 16#80#,
          10 => 16#80#,

          11 => 16#FC#,
          12 => 16#80#,
          13 => 16#80#,
          14 => 16#80#,
          15 => 16#80#,

          16 => 16#DF#,

          17 => 16#EF#,
          18 => 16#BF#,

          19 => 16#F7#,
          20 => 16#BF#,
          21 => 16#BF#,

          22 => 16#FB#,
          23 => 16#BF#,
          24 => 16#BF#,
          25 => 16#BF#,

          26 => 16#FD#,
          27 => 16#BF#,
          28 => 16#BF#,
          29 => 16#BF#,
          30 => 16#BF#)),
      Error_Invalid_1);

   Case_3_5_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#FE#)),
      Error_Invalid_1);

   Case_3_5_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#FF#)),
      Error_Invalid_1);

   Case_3_5_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#FE#,
          2 => 16#FE#,
          3 => 16#FF#,
          4 => 16#FF#)),
      Error_Invalid_1);

   Case_4_1_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#C0#,
          2 => 16#AF#)),
      Error_Invalid_1);

   Case_4_1_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#E0#,
          2 => 16#80#,
          3 => 16#AF#)),
      Error_Invalid_2_3);

   Case_4_1_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F0#,
          2 => 16#80#,
          3 => 16#80#,
          4 => 16#AF#)),
      Error_Invalid_2_4);

   Case_4_1_4 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F8#,
          2 => 16#80#,
          3 => 16#80#,
          4 => 16#80#,
          5 => 16#AF#)),
      Error_Invalid_1);

   Case_4_1_5 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#FC#,
          2 => 16#80#,
          3 => 16#80#,
          4 => 16#80#,
          5 => 16#80#,
          6 => 16#AF#)),
      Error_Invalid_1);

   Case_4_2_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#C1#,
          2 => 16#BF#)),
      Error_Invalid_1);

   Case_4_2_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#E0#,
          2 => 16#9F#,
          3 => 16#BF#)),
      Error_Invalid_2_3);

   Case_4_2_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F0#,
          2 => 16#8F#,
          3 => 16#BF#,
          4 => 16#BF#)),
      Error_Invalid_2_4);

   Case_4_2_4 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F8#,
          2 => 16#87#,
          3 => 16#BF#,
          4 => 16#BF#,
          5 => 16#BF#)),
      Error_Invalid_1);

   Case_4_2_5 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#FC#,
          2 => 16#83#,
          3 => 16#BF#,
          4 => 16#BF#,
          5 => 16#BF#,
          6 => 16#BF#)),
      Error_Invalid_1);

   Case_4_3_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#C0#,
          2 => 16#80#)),
      Error_Invalid_1);

   Case_4_3_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#E0#,
          2 => 16#80#,
          3 => 16#80#)),
      Error_Invalid_2_3);

   Case_4_3_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F0#,
          2 => 16#80#,
          3 => 16#80#,
          4 => 16#80#)),
      Error_Invalid_2_4);

   Case_4_3_4 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#F8#,
          2 => 16#80#,
          3 => 16#80#,
          4 => 16#80#,
          5 => 16#80#)),
      Error_Invalid_1);

   Case_4_3_5 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#FC#,
          2 => 16#80#,
          3 => 16#80#,
          4 => 16#80#,
          5 => 16#80#,
          6 => 16#80#)),
      Error_Invalid_1);

   Case_5_1_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#A0#,
          3 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_1_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AD#,
          3 => 16#BF#)),
      Error_Invalid_2_3);

   Case_5_1_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AE#,
          3 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_1_4 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AF#,
          3 => 16#BF#)),
      Error_Invalid_2_3);

   Case_5_1_5 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#B0#,
          3 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_1_6 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#BE#,
          3 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_1_7 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#BF#,
          3 => 16#BF#)),
      Error_Invalid_2_3);

   Case_5_2_1 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#A0#,
          3 => 16#80#,
          4 => 16#ED#,
          5 => 16#B0#,
          6 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_2_2 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#A0#,
          3 => 16#80#,
          4 => 16#ED#,
          5 => 16#BF#,
          6 => 16#BF#)),
      Error_Invalid_2_3);

   Case_5_2_3 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AD#,
          3 => 16#BF#,
          4 => 16#ED#,
          5 => 16#B0#,
          6 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_2_4 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AD#,
          3 => 16#BF#,
          4 => 16#ED#,
          5 => 16#BF#,
          6 => 16#BF#)),
      Error_Invalid_2_3);

   Case_5_2_5 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AE#,
          3 => 16#80#,
          4 => 16#ED#,
          5 => 16#B0#,
          6 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_2_6 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AE#,
          3 => 16#80#,
          4 => 16#ED#,
          5 => 16#BF#,
          6 => 16#BF#)),
      Error_Invalid_2_3);

   Case_5_2_7 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AF#,
          3 => 16#BF#,
          4 => 16#ED#,
          5 => 16#B0#,
          6 => 16#80#)),
      Error_Invalid_2_3);

   Case_5_2_8 : constant Malformed_Testcase :=
     (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
        ((1 => 16#ED#,
          2 => 16#AF#,
          3 => 16#BF#,
          4 => 16#ED#,
          5 => 16#BF#,
          6 => 16#BF#)),
      Error_Invalid_2_3);

   --  Case_5_3_* are not applicable: noncharacters can be used

   procedure Run_Testcase (Testcase : Correct_Testcase);

   procedure Run_Testcase (Testcase : Malformed_Testcase);

   ------------------
   -- Run_Testcase --
   ------------------

   procedure Run_Testcase (Testcase : Correct_Testcase) is

      use type VSS.Stream_Element_Vectors.Stream_Element_Vector;
      use type VSS.Strings.Virtual_String;

      Input   : VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;
      Output  : VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;
      Text    : VSS.Strings.Virtual_String;
      C       : VSS.Characters.Virtual_Character;
      Success : Boolean := True;

   begin
      Input.Set_Data (Testcase.Data);

      loop
         Input.Get (C, Success);

         if not Success and then Input.Has_Error then
            raise Program_Error;
         end if;

         exit when not Success;

         Text.Append (C);
         Output.Put (C, Success);

         if not Success then
            raise Program_Error;
         end if;
      end loop;

      if Text /= Testcase.Text then
         raise Program_Error;
      end if;

      if Output.Buffer /= Testcase.Data then
         raise Program_Error;
      end if;
   end Run_Testcase;

   ------------------
   -- Run_Testcase --
   ------------------

   procedure Run_Testcase (Testcase : Malformed_Testcase) is

      use type VSS.Characters.Virtual_Character;
      use type VSS.Strings.Virtual_String;

      Input   : VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;
      C       : VSS.Characters.Virtual_Character;
      Success : Boolean := True;

   begin
      Input.Set_Data (Testcase.Data);

      Input.Get (C, Success);

      if Success then
         raise Program_Error;

      elsif C /= VSS.Characters.Virtual_Character'Val (16#0000#) then
         raise Program_Error;

      elsif not Input.Has_Error then
         raise Program_Error;

      elsif Input.Error_Message /= Testcase.Error then
         raise Program_Error;
      end if;
   end Run_Testcase;

begin
   Run_Testcase (Case_1);

   Run_Testcase (Case_2_1_1);
   Run_Testcase (Case_2_1_2);
   Run_Testcase (Case_2_1_3);
   Run_Testcase (Case_2_1_4);

   Run_Testcase (Case_2_2_1);
   Run_Testcase (Case_2_2_2);
   Run_Testcase (Case_2_2_3);

   Run_Testcase (Case_2_3_1);
   Run_Testcase (Case_2_3_2);
   Run_Testcase (Case_2_3_3);
   Run_Testcase (Case_2_3_4);

   Run_Testcase (Case_3_1_1);
   Run_Testcase (Case_3_1_2);
   Run_Testcase (Case_3_1_3);
   Run_Testcase (Case_3_1_4);
   Run_Testcase (Case_3_1_5);
   Run_Testcase (Case_3_1_6);
   Run_Testcase (Case_3_1_7);
   Run_Testcase (Case_3_1_8);
   Run_Testcase (Case_3_1_9);

   Run_Testcase (Case_3_2_1);
   Run_Testcase (Case_3_2_2);
   Run_Testcase (Case_3_2_3);

   Run_Testcase (Case_3_3_1);
   Run_Testcase (Case_3_3_2);
   Run_Testcase (Case_3_3_3);
   Run_Testcase (Case_3_3_6);
   Run_Testcase (Case_3_3_7);
   Run_Testcase (Case_3_3_8);

   Run_Testcase (Case_3_4);

   Run_Testcase (Case_3_5_1);
   Run_Testcase (Case_3_5_2);
   Run_Testcase (Case_3_5_3);

   Run_Testcase (Case_4_1_1);
   Run_Testcase (Case_4_1_2);
   Run_Testcase (Case_4_1_3);
   Run_Testcase (Case_4_1_4);
   Run_Testcase (Case_4_1_5);

   Run_Testcase (Case_4_2_1);
   Run_Testcase (Case_4_2_2);
   Run_Testcase (Case_4_2_3);
   Run_Testcase (Case_4_2_4);
   Run_Testcase (Case_4_2_5);

   Run_Testcase (Case_4_3_1);
   Run_Testcase (Case_4_3_2);
   Run_Testcase (Case_4_3_3);
   Run_Testcase (Case_4_3_4);
   Run_Testcase (Case_4_3_5);

   Run_Testcase (Case_5_1_1);
   Run_Testcase (Case_5_1_2);
   Run_Testcase (Case_5_1_3);
   Run_Testcase (Case_5_1_4);
   Run_Testcase (Case_5_1_5);
   Run_Testcase (Case_5_1_6);
   Run_Testcase (Case_5_1_7);

   Run_Testcase (Case_5_2_1);
   Run_Testcase (Case_5_2_2);
   Run_Testcase (Case_5_2_3);
   Run_Testcase (Case_5_2_4);
   Run_Testcase (Case_5_2_5);
   Run_Testcase (Case_5_2_6);
   Run_Testcase (Case_5_2_7);
   Run_Testcase (Case_5_2_8);
end Markus_Kuhn_Test;
