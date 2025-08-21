--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

separate (Test_Converters)
procedure Test_EUCJP_Decoder is

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
      Run_Decoder_Test ("EUC-JP", Encoded, Decoded, Has_Error, Comment);
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
      Run_Decoder_Test ("EUC-JP", Encoded, Decoded, Has_Error, Comment);
   end Test;

begin
   --  Tests based on Web Platform Tests, see
   --
   --  https://github.com/web-platform-tests/wpt

   --  Tests for error cases

   Test (16#B0#, "�", True, "lead not 0x00 and no more bytes: B0");
   Test
     ([16#B0#, 16#B5#, 16#B0#],
      "圧�",
      True,
      "lead not 0x00 and no more bytes: B0 B5 B0");
   Test
     ([16#B0#, 16#B0#, 16#B0#],
      "旭�",
      True,
      "lead not 0x00 and no more bytes: B0 B0 B0");
   Test
     (16#FE#,
      "�",
      True,
      "lead byte outside 0x8E, 0x8F, or the range 0xA1 to 0xFE: FF");
   Test
     ([16#B0#, 16#B5#, 16#FF#],
      "圧�",
      True,
      "lead byte outside 0x8E, 0x8F, or the range 0xA1 to 0xFE: B0 B5 FF");
   Test
     (16#91#,
      "�",
      True,
      "lead byte outside 0x8E, 0x8F, or the range 0xA1 to 0xFE: 91");
   Test
     ([16#B0#, 16#B5#, 16#91#],
      "圧�",
      True,
      "lead byte outside 0x8E, 0x8F, or the range 0xA1 to 0xFE: B0 B5 91");
   Test ([16#B0#, 16#31#], "�1", True, "trail byte outside 0xA1-0xFE: B0 31");
   Test ([16#B0#, 16#FF#], "�", True, "trail byte outside 0xA1-0xFE: B0 FF");
   Test ([16#B0#, 16#A0#], "�", True, "trail byte outside 0xA1-0xFE: B0 A0");
   Test ([16#A2#, 16#B9#], "�", True, "pointer is null: A2 B9");

   --  More tests are run separately with data files.
end Test_EUCJP_Decoder;
