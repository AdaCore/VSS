--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces;

with VSS.Characters;
with VSS.Unicode;

separate (Test_Converters)
procedure Test_GB18030_Decoder is

   type Code_Point_Array is
     array (Positive range <>) of VSS.Unicode.Code_Point;

   procedure Test
     (Byte       : Ada.Streams.Stream_Element;
      Code_Point : VSS.Unicode.Code_Point;
      Has_Error  : Boolean;
      Comment    : VSS.Strings.Virtual_String);

   procedure Test
     (Encoded    : Ada.Streams.Stream_Element_Array;
      Code_Point : VSS.Unicode.Code_Point;
      Has_Error  : Boolean;
      Comment    : VSS.Strings.Virtual_String);

   procedure Test
     (Encoded     : Ada.Streams.Stream_Element_Array;
      Code_Points : Code_Point_Array;
      Has_Error   : Boolean;
      Comment     : VSS.Strings.Virtual_String);

   ----------
   -- Test --
   ----------

   procedure Test
     (Byte       : Ada.Streams.Stream_Element;
      Code_Point : VSS.Unicode.Code_Point;
      Has_Error  : Boolean;
      Comment    : VSS.Strings.Virtual_String)
   is
      Encoded     : constant Ada.Streams.Stream_Element_Array (1 .. 1) :=
        (1 => Byte);
      Code_Points : constant Code_Point_Array (1 .. 1) := (1 => Code_Point);

   begin
      Test (Encoded, Code_Points, Has_Error, Comment);
   end Test;

   ----------
   -- Test --
   ----------

   procedure Test
     (Encoded    : Ada.Streams.Stream_Element_Array;
      Code_Point : VSS.Unicode.Code_Point;
      Has_Error  : Boolean;
      Comment    : VSS.Strings.Virtual_String)
   is
      Code_Points : constant Code_Point_Array (1 .. 1) := (1 => Code_Point);

   begin
      Test (Encoded, Code_Points, Has_Error, Comment);
   end Test;

   ----------
   -- Test --
   ----------

   procedure Test
     (Encoded     : Ada.Streams.Stream_Element_Array;
      Code_Points : Code_Point_Array;
      Has_Error   : Boolean;
      Comment     : VSS.Strings.Virtual_String)
   is
      Decoded : VSS.Strings.Virtual_String;

   begin
      for Item of Code_Points loop
         Decoded.Append (VSS.Characters.Virtual_Character'Val (Item));
      end loop;

      Run_Decoder_Test ("gb18030", Encoded, Decoded, Has_Error, Comment);
   end Test;

   type Range_Record is record
      Pointer    : Interfaces.Unsigned_32;
      Code_Point : VSS.Unicode.Code_Point;
   end record;

   Ranges : constant array (Natural range <>) of Range_Record :=
     ((0,      16#0080#),
      (36,     16#00A5#),
      (38,     16#00A9#),
      (45,     16#00B2#),
      (50,     16#00B8#),
      (81,     16#00D8#),
      (89,     16#00E2#),
      (95,     16#00EB#),
      (96,     16#00EE#),
      (100,    16#00F4#),
      (103,    16#00F8#),
      (104,    16#00FB#),
      (105,    16#00FD#),
      (109,    16#0102#),
      (126,    16#0114#),
      (133,    16#011C#),
      (148,    16#012C#),
      (172,    16#0145#),
      (175,    16#0149#),
      (179,    16#014E#),
      (208,    16#016C#),
      (306,    16#01CF#),
      (307,    16#01D1#),
      (308,    16#01D3#),
      (309,    16#01D5#),
      (310,    16#01D7#),
      (311,    16#01D9#),
      (312,    16#01DB#),
      (313,    16#01DD#),
      (341,    16#01FA#),
      (428,    16#0252#),
      (443,    16#0262#),
      (544,    16#02C8#),
      (545,    16#02CC#),
      (558,    16#02DA#),
      (741,    16#03A2#),
      (742,    16#03AA#),
      (749,    16#03C2#),
      (750,    16#03CA#),
      (805,    16#0402#),
      (819,    16#0450#),
      (820,    16#0452#),
      (7922,   16#2011#),
      (7924,   16#2017#),
      (7925,   16#201A#),
      (7927,   16#201E#),
      (7934,   16#2027#),
      (7943,   16#2031#),
      (7944,   16#2034#),
      (7945,   16#2036#),
      (7950,   16#203C#),
      (8062,   16#20AD#),
      (8148,   16#2104#),
      (8149,   16#2106#),
      (8152,   16#210A#),
      (8164,   16#2117#),
      (8174,   16#2122#),
      (8236,   16#216C#),
      (8240,   16#217A#),
      (8262,   16#2194#),
      (8264,   16#219A#),
      (8374,   16#2209#),
      (8380,   16#2210#),
      (8381,   16#2212#),
      (8384,   16#2216#),
      (8388,   16#221B#),
      (8390,   16#2221#),
      (8392,   16#2224#),
      (8393,   16#2226#),
      (8394,   16#222C#),
      (8396,   16#222F#),
      (8401,   16#2238#),
      (8406,   16#223E#),
      (8416,   16#2249#),
      (8419,   16#224D#),
      (8424,   16#2253#),
      (8437,   16#2262#),
      (8439,   16#2268#),
      (8445,   16#2270#),
      (8482,   16#2296#),
      (8485,   16#229A#),
      (8496,   16#22A6#),
      (8521,   16#22C0#),
      (8603,   16#2313#),
      (8936,   16#246A#),
      (8946,   16#249C#),
      (9046,   16#254C#),
      (9050,   16#2574#),
      (9063,   16#2590#),
      (9066,   16#2596#),
      (9076,   16#25A2#),
      (9092,   16#25B4#),
      (9100,   16#25BE#),
      (9108,   16#25C8#),
      (9111,   16#25CC#),
      (9113,   16#25D0#),
      (9131,   16#25E6#),
      (9162,   16#2607#),
      (9164,   16#260A#),
      (9218,   16#2641#),
      (9219,   16#2643#),
      (11329,  16#2E82#),
      (11331,  16#2E85#),
      (11334,  16#2E89#),
      (11336,  16#2E8D#),
      (11346,  16#2E98#),
      (11361,  16#2EA8#),
      (11363,  16#2EAB#),
      (11366,  16#2EAF#),
      (11370,  16#2EB4#),
      (11372,  16#2EB8#),
      (11375,  16#2EBC#),
      (11389,  16#2ECB#),
      (11682,  16#2FFC#),
      (11686,  16#3004#),
      (11687,  16#3018#),
      (11692,  16#301F#),
      (11694,  16#302A#),
      (11714,  16#303F#),
      (11716,  16#3094#),
      (11723,  16#309F#),
      (11725,  16#30F7#),
      (11730,  16#30FF#),
      (11736,  16#312A#),
      (11982,  16#322A#),
      (11989,  16#3232#),
      (12102,  16#32A4#),
      (12336,  16#3390#),
      (12348,  16#339F#),
      (12350,  16#33A2#),
      (12384,  16#33C5#),
      (12393,  16#33CF#),
      (12395,  16#33D3#),
      (12397,  16#33D6#),
      (12510,  16#3448#),
      (12553,  16#3474#),
      (12851,  16#359F#),
      (12962,  16#360F#),
      (12973,  16#361B#),
      (13738,  16#3919#),
      (13823,  16#396F#),
      (13919,  16#39D1#),
      (13933,  16#39E0#),
      (14080,  16#3A74#),
      (14298,  16#3B4F#),
      (14585,  16#3C6F#),
      (14698,  16#3CE1#),
      (15583,  16#4057#),
      (15847,  16#4160#),
      (16318,  16#4338#),
      (16434,  16#43AD#),
      (16438,  16#43B2#),
      (16481,  16#43DE#),
      (16729,  16#44D7#),
      (17102,  16#464D#),
      (17122,  16#4662#),
      (17315,  16#4724#),
      (17320,  16#472A#),
      (17402,  16#477D#),
      (17418,  16#478E#),
      (17859,  16#4948#),
      (17909,  16#497B#),
      (17911,  16#497E#),
      (17915,  16#4984#),
      (17916,  16#4987#),
      (17936,  16#499C#),
      (17939,  16#49A0#),
      (17961,  16#49B8#),
      (18664,  16#4C78#),
      (18703,  16#4CA4#),
      (18814,  16#4D1A#),
      (18962,  16#4DAF#),
      (19043,  16#9FA6#),
      (33469,  16#E76C#),
      (33470,  16#E7C8#),
      (33471,  16#E7E7#),
      (33484,  16#E815#),
      (33485,  16#E819#),
      (33490,  16#E81F#),
      (33497,  16#E827#),
      (33501,  16#E82D#),
      (33505,  16#E833#),
      (33513,  16#E83C#),
      (33520,  16#E844#),
      (33536,  16#E856#),
      (33550,  16#E865#),
      (37845,  16#F92D#),
      (37921,  16#F97A#),
      (37948,  16#F996#),
      (38029,  16#F9E8#),
      (38038,  16#F9F2#),
      (38064,  16#FA10#),
      (38065,  16#FA12#),
      (38066,  16#FA15#),
      (38069,  16#FA19#),
      (38075,  16#FA22#),
      (38076,  16#FA25#),
      (38078,  16#FA2A#),
      (39108,  16#FE32#),
      (39109,  16#FE45#),
      (39113,  16#FE53#),
      (39114,  16#FE58#),
      (39115,  16#FE67#),
      (39116,  16#FE6C#),
      (39265,  16#FF5F#),
      (39394,  16#FFE6#),
      (189000, 16#1_0000#));

begin
   --  Tests based on Web Platform Tests, see
   --
   --  https://github.com/web-platform-tests/wpt

   Test (115, VSS.Characters.Virtual_Character'Pos ('s'), False, "ASCII");
   Test (16#80#, 16#20AC#, False, "euro");
   Test (16#FF#, 16#FFFD#, True, "initial byte out of accepted ranges");
   Test (16#81#, 16#FFFD#, True, "end of queue, gb18030 first not 0");
   Test
     ((16#81#, 16#28#),
      (16#FFFD#, VSS.Characters.Virtual_Character'Pos ('(')),
      True,
      "two bytes 0x81 0x28");
   Test ((16#81#, 16#40#), 16#4E02#, False, "two bytes 0x81 0x40");
   Test ((16#81#, 16#7E#), 16#4E8A#, False, "two bytes 0x81 0x7e");
   Test ((16#81#, 16#7F#), (16#FFFD#, 16#007F#), True, "two bytes 0x81 0x7f");
   Test ((16#81#, 16#80#), 16#4E90#, False, "two bytes 0x81 0x80");
   Test ((16#81#, 16#FE#), 16#4FA2#, False, "two bytes 0x81 0xFE");
   Test ((16#81#, 16#FF#), 16#FFFD#, True, "two bytes 0x81 0xFF");
   Test ((16#FE#, 16#40#), 16#FA0C#, False, "two bytes 0xFE 0x40");
   Test ((16#FE#, 16#FE#), 16#E4C5#, False, "two bytes 0xFE 0xFE");
   Test ((16#FE#, 16#FF#), 16#FFFD#, True, "two bytes 0xFE 0xFF");
   Test ((16#81#, 16#30#), 16#FFFD#, True, "two bytes 0x81 0x30");
   Test
     ((16#81#, 16#30#, 16#FE#), 16#FFFD#, True, "three bytes 0x81 0x30 0xFE");
   Test
     ((16#81#, 16#30#, 16#FF#),
      (16#FFFD#, VSS.Characters.Virtual_Character'Pos ('0'), 16#FFFD#),
      True,
      "three bytes 0x81 0x30 0xFF");
   Test
     ((16#81#, 16#30#, 16#FE#, 16#29#),
      (16#FFFD#,
       VSS.Characters.Virtual_Character'Pos ('0'),
       16#FFFD#,
       VSS.Characters.Virtual_Character'Pos (')')),
      True,
      "four bytes 0x81 0x30 0xFE 0x29");
   Test
     ((16#FE#, 16#39#, 16#FE#, 16#39#),
      16#FFFD#,
      True,
      "four bytes 0xFE 0x39 0xFE 0x39");
   Test
     ((16#81#, 16#35#, 16#F4#, 16#36#),
      16#1E3E#,
      False,
      "pointer 7458");
   Test ((16#81#, 16#35#, 16#F4#, 16#37#), 16#E7C7#, False, "pointer 7457");
   Test ((16#81#, 16#35#, 16#F4#, 16#38#), 16#1E40#, False, "pointer 7459");
   Test ((16#84#, 16#31#, 16#A4#, 16#39#), 16#FFFF#, False, "pointer 39419");
   Test ((16#84#, 16#31#, 16#A5#, 16#30#), 16#FFFD#, True, "pointer 39420");
   Test ((16#8F#, 16#39#, 16#FE#, 16#39#), 16#FFFD#, True, "pointer 189999");
   Test
     ((16#90#, 16#30#, 16#81#, 16#30#), 16#1_0000#, False, "pointer 189000");
   Test
     ((16#E3#, 16#32#, 16#9A#, 16#35#), 16#10_FFFF#, False, "pointer 1237575");
   Test ((16#E3#, 16#32#, 16#9A#, 16#36#), 16#FFFD#, True, "pointer 1237576");
   Test
     ((16#83#, 16#36#, 16#C8#, 16#30#),
      16#E7C8#,
      False,
      "legacy ICU special case 1");
   Test ((16#A1#, 16#AD#), 16#2026#, False, "legacy ICU special case 2");
   Test ((16#A1#, 16#AB#), 16#FF5E#, False, "legacy ICU special case 3");

   for J in Ranges'Range loop
      declare
         use type Interfaces.Unsigned_32;

         Pointer : constant Interfaces.Unsigned_32 := Ranges (J).Pointer;
         Encoded : constant Ada.Streams.Stream_Element_Array (1 .. 4) :=
           (Ada.Streams.Stream_Element (Pointer / 12600 + 16#81#),
            Ada.Streams.Stream_Element ((Pointer mod 12600) / 1260 + 16#30#),
            Ada.Streams.Stream_Element ((Pointer mod 1260) / 10 + 16#81#),
            Ada.Streams.Stream_Element (Pointer mod 10 + 16#30#));

      begin
         Test
           (Encoded,
            Ranges (J).Code_Point,
            False,
            VSS.Strings.To_Virtual_String
              ("range" & Natural'Wide_Wide_Image (J)));
      end;
   end loop;
end Test_GB18030_Decoder;
