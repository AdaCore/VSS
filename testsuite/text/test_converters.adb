--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;

with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Converters.Decoders;
with VSS.Strings.Converters.Encoders;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_Converters is

   use type VSS.Stream_Element_Vectors.Stream_Element_Vector;
   use all type VSS.Strings.Converters.Converter_Flag;

   D1 : constant Ada.Streams.Stream_Element_Array :=
     (16#EF#, 16#BB#, 16#BF#);
   --  BOM only

   D2 : constant Ada.Streams.Stream_Element_Array :=
     (16#C0#, 16#AF#, 16#E0#, 16#80#,  16#BF#, 16#F0#, 16#81#, 16#82#,
      16#41#);
   E2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#)));
   --  Unicode 13.0, Table 3-8.

   D3 : constant Ada.Streams.Stream_Element_Array :=
     (16#ED#, 16#A0#, 16#80#, 16#ED#,  16#BF#, 16#BF#, 16#ED#, 16#AF#,
      16#41#);
   E3 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#)));
   --  Unicode 13.0, Table 3-9.

   D4 : constant Ada.Streams.Stream_Element_Array :=
     (16#F4#, 16#91#, 16#92#, 16#93#,  16#FF#, 16#41#, 16#80#, 16#BF#,
      16#42#);
   E4 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0042#)));
   --  Unicode 13.0, Table 3-10.

   D5 : constant Ada.Streams.Stream_Element_Array :=
     (16#E1#, 16#80#, 16#E2#, 16#F0#,  16#91#, 16#92#, 16#F1#, 16#BF#,
      16#41#);
   E5 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ((Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#FFFD#),
         Wide_Wide_Character'Val (16#0041#)));
   --  Unicode 13.0, Table 3-11.

   D6 : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#, 16#91#, 16#E0#,  16#A4#, 16#95#, 16#F0#, 16#90#,
      16#8C#, 16#88#);
   E6 : constant VSS.Strings.Virtual_String := "AБक𐌈";
   --  Single character from of each number of bytes of encoded sequence.

   D7_2  : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#);
   E7_2  : constant VSS.Strings.Virtual_String := "A�";
   D7_31 : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#, 16#91#, 16#E0#);
   D7_32 : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#, 16#91#, 16#E0#,  16#A4#);
   E7_3  : constant VSS.Strings.Virtual_String := "AБ�";
   D7_41 : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#, 16#91#, 16#E0#,  16#A4#, 16#95#, 16#F0#);
   D7_42 : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#, 16#91#, 16#E0#,  16#A4#, 16#95#, 16#F0#, 16#90#);
   D7_43 : constant Ada.Streams.Stream_Element_Array :=
     (16#41#, 16#D0#, 16#91#, 16#E0#,  16#A4#, 16#95#, 16#F0#, 16#90#,
      16#8C#);
   E7_4  : constant VSS.Strings.Virtual_String := "AБक�";
   --  Incomplete multibyte seqence at the end of the encoded data.

   procedure Run_Decoder_Test
     (Encoding  : VSS.Strings.Virtual_String;
      Encoded   : Ada.Streams.Stream_Element_Array;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Comment   : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Run decoder for given encoding in two modes: block and incremental,
   --  and check result.

   procedure Run_Encoder_Test
     (Encoded : Ada.Streams.Stream_Element_Array;
      Decoded : VSS.Strings.Virtual_String);
   --  Run encoder for UTF-8 encoding in two modes: block and incremental,
   --  and check result.

   procedure Test_GB18030_Decoder;
   --  Run tests of the GB-18030 decoder. It tests GBK, GB2312 encodings too.

   procedure Test_EUCJP_Decoder;
   --  Run tests of the EUC-JP decoder.

   ----------------------
   -- Run_Decoder_Test --
   ----------------------

   procedure Run_Decoder_Test
     (Encoding  : VSS.Strings.Virtual_String;
      Encoded   : Ada.Streams.Stream_Element_Array;
      Decoded   : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Comment   : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      Message : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Comment);

   begin
      --  Stream_Element_Array and block mode

      declare
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize (Encoding);

         Test_Support.Assert (Decoder.Is_Valid, Message);

         Result := Decoder.Decode (Encoded);

         Test_Support.Assert (Result = Decoded, Message);
         Test_Support.Assert (Decoder.Has_Error = Has_Error, Message);
         Test_Support.Assert
           (Decoder.Error_Message.Is_Empty xor Has_Error, Message);
      end;

      --  Stream_Element_Vector and block mode

      declare
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize (Encoding);

         if not Decoder.Is_Valid then
            raise Program_Error;
         end if;

         Result :=
           Decoder.Decode
             (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
                (Encoded));

         if Result /= Decoded then
            raise Program_Error;
         end if;

         if Decoder.Has_Error /= Has_Error then
            raise Program_Error;
         end if;

         if Decoder.Error_Message.Is_Empty and Has_Error then
            raise Program_Error;
         end if;
      end;

      --  Stream_Element_Array and incremental mode

      declare
         use type Ada.Streams.Stream_Element_Offset;
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize (Encoding);

         Test_Support.Assert (Decoder.Is_Valid);

         for J in Encoded'Range loop
            Result.Append
              (Decoder.Decode
                 (Ada.Streams.Stream_Element_Array'((1 => Encoded (J))),
                  J = Encoded'Last));
         end loop;

         Test_Support.Assert (Result = Decoded);

         Test_Support.Assert (Decoder.Has_Error = Has_Error);

         if Decoder.Error_Message.Is_Empty and Has_Error then
            raise Program_Error;
         end if;
      end;

      --  Stream_Element_Vector and incremental mode

      declare
         use type Ada.Streams.Stream_Element_Offset;
         use type VSS.Strings.Virtual_String;

         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Result  : VSS.Strings.Virtual_String;

      begin
         Decoder.Initialize (Encoding);

         Test_Support.Assert (Decoder.Is_Valid);

         for J in Encoded'Range loop
            Result.Append
              (Decoder.Decode
                 (VSS.Stream_Element_Vectors.Conversions
                    .To_Stream_Element_Vector ((1 => Encoded (J))),
                  J = Encoded'Last));
         end loop;

         Test_Support.Assert (Result = Decoded);

         Test_Support.Assert (Decoder.Has_Error = Has_Error);

         if Decoder.Error_Message.Is_Empty and Has_Error then
            raise Program_Error;
         end if;
      end;
   end Run_Decoder_Test;

   ----------------------
   -- Run_Encoder_Test --
   ----------------------

   procedure Run_Encoder_Test
     (Encoded : Ada.Streams.Stream_Element_Array;
      Decoded : VSS.Strings.Virtual_String)
   is
      Encoded_Vector :
        constant VSS.Stream_Element_Vectors.Stream_Element_Vector :=
          VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
            (Encoded);

   begin
      --  Encode: function & string

      declare
         Encoder : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
         Result  : VSS.Stream_Element_Vectors.Stream_Element_Vector;

      begin
         Encoder.Initialize ("utf-8");

         Test_Support.Assert (Encoder.Is_Valid);

         Result := Encoder.Encode (Decoded);

         Test_Support.Assert (Result = Encoded_Vector);
         Test_Support.Assert (not Encoder.Has_Error);
         Test_Support.Assert (Encoder.Error_Message.Is_Empty);
      end;

      --  Encode: procedure & string

      declare
         Encoder : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
         Result  : VSS.Stream_Element_Vectors.Stream_Element_Vector;

      begin
         Encoder.Initialize ("utf-8");

         Test_Support.Assert (Encoder.Is_Valid);

         Encoder.Encode (Decoded, Result);

         Test_Support.Assert (Result = Encoded_Vector);
         Test_Support.Assert (not Encoder.Has_Error);
         Test_Support.Assert (Encoder.Error_Message.Is_Empty);
      end;

      --  Encode: function & character

      declare
         Encoder  : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
         Result   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
         Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
           Decoded.Before_First_Character;

      begin
         Encoder.Initialize ("utf-8");

         Test_Support.Assert (Encoder.Is_Valid);

         while Iterator.Forward loop
            Result.Append (Encoder.Encode (Iterator.Element));
         end loop;

         Test_Support.Assert (Result = Encoded_Vector);
         Test_Support.Assert (not Encoder.Has_Error);
         Test_Support.Assert (Encoder.Error_Message.Is_Empty);
      end;

      --  Encode: procedure & character

      declare
         Encoder  : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
         Result   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
         Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
           Decoded.Before_First_Character;

      begin
         Encoder.Initialize ("utf-8");

         Test_Support.Assert (Encoder.Is_Valid);

         while Iterator.Forward loop
            Encoder.Encode (Iterator.Element, Result);
         end loop;

         Test_Support.Assert (Result = Encoded_Vector);
         Test_Support.Assert (not Encoder.Has_Error);
         Test_Support.Assert (Encoder.Error_Message.Is_Empty);
      end;
   end Run_Encoder_Test;

   ------------------------
   -- Test_EUCJP_Decoder --
   ------------------------

   procedure Test_EUCJP_Decoder is separate;

   --------------------------
   -- Test_GB18030_Decoder --
   --------------------------

   procedure Test_GB18030_Decoder is separate;

begin
   --  Check invalid state of the decoder after object declaration without
   --  initialization.

   declare
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;

   begin
      if Decoder.Is_Valid then
         raise Program_Error;
      end if;
   end;

   --  Check conversion of empty data

   declare
      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;
      E : VSS.Stream_Element_Vectors.Stream_Element_Vector;

   begin
      D.Initialize ("utf-8");

      if not D.Is_Valid then
         raise Program_Error;
      end if;

      S := D.Decode (E);

      if not S.Is_Empty then
         raise Program_Error;
      end if;
   end;

   --  Check processing of the BOM

   declare
      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Process_BOM => True, others => False));

      if not D.Is_Valid then
         raise Program_Error;
      end if;

      S := D.Decode (D1);

      if not S.Is_Empty then
         raise Program_Error;
      end if;
   end;

   declare
      use type Ada.Streams.Stream_Element_Offset;

      D : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      S : VSS.Strings.Virtual_String;

   begin
      D.Initialize ("utf-8", (Process_BOM => True, others => False));

      Test_Support.Assert (D.Is_Valid);

      for J in D1'Range loop
         S.Append
           (D.Decode
              (Ada.Streams.Stream_Element_Array'((1 => D1 (J))),
               J = D1'Last));
      end loop;

      Test_Support.Assert (S.Is_Empty);
   end;

   Run_Decoder_Test ("utf-8", D2, E2, True);
   Run_Decoder_Test ("utf-8", D3, E3, True);
   Run_Decoder_Test ("utf-8", D4, E4, True);
   Run_Decoder_Test ("utf-8", D5, E5, True);
   Run_Decoder_Test ("utf-8", D6, E6, False);

   --  Check processing of the BOM and empty string

   declare
      Encoder        : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
      Encoded_Vector :
        constant VSS.Stream_Element_Vectors.Stream_Element_Vector :=
          VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
           (D1);
      Result         : VSS.Stream_Element_Vectors.Stream_Element_Vector;

   begin
      Encoder.Initialize ("utf-8", (Process_BOM => True, others => False));

      Test_Support.Assert (Encoder.Is_Valid);

      Result := Encoder.Encode (VSS.Strings.Empty_Virtual_String);

      Test_Support.Assert (Result = Encoded_Vector);
      Test_Support.Assert (not Encoder.Has_Error);
      Test_Support.Assert (Encoder.Error_Message.Is_Empty);
   end;

   --  Check processing of the BOM and non empty string

   declare
      use type Ada.Streams.Stream_Element_Array;

      Encoder        : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
      Encoded_Vector :
        constant VSS.Stream_Element_Vectors.Stream_Element_Vector :=
          VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
           (D1 & D6);
      Result         : VSS.Stream_Element_Vectors.Stream_Element_Vector;

   begin
      Encoder.Initialize ("utf-8", (Process_BOM => True, others => False));

      Test_Support.Assert (Encoder.Is_Valid);

      Result := Encoder.Encode (E6);

      Test_Support.Assert (Result = Encoded_Vector);
      Test_Support.Assert (not Encoder.Has_Error);
      Test_Support.Assert (Encoder.Error_Message.Is_Empty);
   end;

   --  Check processing of the BOM and single character

   declare
      use type Ada.Streams.Stream_Element_Array;

      Encoder        : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;
      Encoded_Vector :
        constant VSS.Stream_Element_Vectors.Stream_Element_Vector :=
          VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
           (D1 & Character'Pos ('Z'));
      Result         : VSS.Stream_Element_Vectors.Stream_Element_Vector;

   begin
      Encoder.Initialize ("utf-8", (Process_BOM => True, others => False));

      Test_Support.Assert (Encoder.Is_Valid);

      Result := Encoder.Encode ('Z');

      Test_Support.Assert (Result = Encoded_Vector);
      Test_Support.Assert (not Encoder.Has_Error);
      Test_Support.Assert (Encoder.Error_Message.Is_Empty);
   end;

   Run_Encoder_Test (D6, E6);

   --  Check reporting of the error at the end of the incomplete mutlubyte
   --  sequence at the end of the data.

   Run_Decoder_Test ("utf-8", D7_2, E7_2, True);
   Run_Decoder_Test ("utf-8", D7_31, E7_3, True);
   Run_Decoder_Test ("utf-8", D7_32, E7_3, True);
   Run_Decoder_Test ("utf-8", D7_41, E7_4, True);
   Run_Decoder_Test ("utf-8", D7_42, E7_4, True);
   Run_Decoder_Test ("utf-8", D7_43, E7_4, True);

   --  Test other decoders.

   Test_GB18030_Decoder;
   Test_EUCJP_Decoder;
end Test_Converters;
