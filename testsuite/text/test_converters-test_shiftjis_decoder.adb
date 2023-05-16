--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

separate (Test_Converters)
procedure Test_ShiftJIS_Decoder is

   procedure Test
     (Byte      : Ada.Streams.Stream_Element;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Comment   : VSS.Strings.Virtual_String);

   procedure Test
     (Encoded   : Ada.Streams.Stream_Element_Array;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Comment   : VSS.Strings.Virtual_String);

   ----------
   -- Test --
   ----------

   procedure Test
     (Byte      : Ada.Streams.Stream_Element;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Comment   : VSS.Strings.Virtual_String)
   is
      Encoded : constant Ada.Streams.Stream_Element_Array (1 .. 1) :=
        [1 => Byte];

   begin
      Run_Decoder_Test ("Shift-JIS", Encoded, Decoded, Has_Error, Comment);
   end Test;

   ----------
   -- Test --
   ----------

   procedure Test
     (Encoded   : Ada.Streams.Stream_Element_Array;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Comment   : VSS.Strings.Virtual_String) is
   begin
      Run_Decoder_Test ("Shift-JIS", Encoded, Decoded, Has_Error, Comment);
   end Test;

begin
   --  Tests based on Web Platform Tests, see
   --
   --  https://github.com/web-platform-tests/wpt

   --  Tests for error cases

   Test (16#FA#, "�", True, "lead not 0x00 and no more bytes: FA");
   Test
     ([16#FA#, 16#6E#, 16#FA#],
      "佖�",
      True,
      "lead not 0x00 and no more bytes: FA 6E FA");
   Test
     ([16#FA#, 16#FA#, 16#FA#],
      "洄�",
      True,
      "lead not 0x00 and no more bytes: FA FA FA");
   Test
     (16#FF#,
      "�",
      True,
      "lead byte outside 0x81-0x9F,0xA1-0xDF,0xE0,0xFC: FF");
   Test
     (16#A0#,
      "�",
      True,
      "lead byte outside 0x81-0x9F,0xA1-0xDF,0xE0,0xFC: A0");
   Test
     (16#E1#,
      "�",
      True,
      "lead byte outside 0x81-0x9F,0xA1-0xDF,0xE0,0xFC: E1");
   Test
     ([16#FA#, 16#FA#, 16#FF#],
      "洄�",
      True,
      "lead byte outside 0x81-0x9F,0xA1-0xDF,0xE0,0xFC: FA FA FF");
   Test
     ([16#FA#, 16#FD#],
      "�",
      True,
      "trail byte outside 0x41-0xFE: FA FD");
   Test
     ([16#FA#, 16#FE#],
      "�",
      True,
      "trail byte outside 0x41-0xFE: FA FE");
   Test
     ([16#81#, 16#B5#],
      "�",
      True,
      "pointer is null: 81 B5");

   --  More tests are run separately with data files.
end Test_ShiftJIS_Decoder;
