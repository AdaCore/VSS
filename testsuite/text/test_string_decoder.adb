--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams.Stream_IO;

with VSS.Application;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Converters.Decoders;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_String_Decoder is

   use type VSS.Strings.Virtual_String;

   function Load
     (File_Name : VSS.Strings.Virtual_String)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector;

   procedure Run_Decoder_Block_Mode
     (Encoding  : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Encoded   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Decoded   : VSS.Strings.Virtual_String);

   procedure Run_Decoder_Incremental_Mode
     (Encoding  : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Encoded   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Decoded   : VSS.Strings.Virtual_String);

   ----------
   -- Load --
   ----------

   function Load
     (File_Name : VSS.Strings.Virtual_String)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector
   is
      use type Ada.Streams.Stream_Element_Offset;

      File   : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last   : Ada.Streams.Stream_Element_Offset;

   begin
      return Result : VSS.Stream_Element_Vectors.Stream_Element_Vector do
         Ada.Streams.Stream_IO.Open
           (File,
            Ada.Streams.Stream_IO.In_File,
            VSS.Strings.Conversions.To_UTF_8_String (File_Name));

         loop
            Ada.Streams.Stream_IO.Read (File, Buffer, Last);

            exit when Last < Buffer'First;

            Result.Append
              (VSS.Stream_Element_Vectors.Conversions.To_Stream_Element_Vector
                 (Buffer (Buffer'First .. Last)));
         end loop;

         Ada.Streams.Stream_IO.Close (File);
      end return;
   end Load;

   ----------------------------
   -- Run_Decoder_Block_Mode --
   ----------------------------

   procedure Run_Decoder_Block_Mode
     (Encoding  : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Encoded   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Decoded   : VSS.Strings.Virtual_String)
   is
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      Result  : VSS.Strings.Virtual_String;

   begin
      Decoder.Initialize (Encoding);

      Test_Support.Assert (Decoder.Is_Valid);

      Result := Decoder.Decode (Encoded);

      Test_Support.Assert (Result = Decoded);
      Test_Support.Assert (Decoder.Has_Error = Has_Error);

      if Has_Error then
         Test_Support.Assert (not Decoder.Error_Message.Is_Empty);

      else
         Test_Support.Assert (Decoder.Error_Message.Is_Empty);
      end if;
   end Run_Decoder_Block_Mode;

   ----------------------------------
   -- Run_Decoder_Incremental_Mode --
   ----------------------------------

   procedure Run_Decoder_Incremental_Mode
     (Encoding  : VSS.Strings.Virtual_String;
      Has_Error : Boolean;
      Encoded   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Decoded   : VSS.Strings.Virtual_String)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      Result  : VSS.Strings.Virtual_String;

   begin
      Decoder.Initialize (Encoding);

      Test_Support.Assert (Decoder.Is_Valid);

      for J in 1 .. Encoded.Length loop
         Result.Append
           (Decoder.Decode
              (VSS.Stream_Element_Vectors.Conversions
                 .To_Stream_Element_Vector ((1 => Encoded (J))),
               J = Encoded.Length));
      end loop;

      Test_Support.Assert (Result = Decoded);
      Test_Support.Assert (Decoder.Has_Error = Has_Error);

      if Has_Error then
         Test_Support.Assert (not Decoder.Error_Message.Is_Empty);

      else
         Test_Support.Assert (Decoder.Error_Message.Is_Empty);
      end if;
   end Run_Decoder_Incremental_Mode;

begin
   declare
      Encoding  : constant VSS.Strings.Virtual_String :=
        VSS.Application.Arguments.Element (1);
      Has_Error : constant Boolean :=
        Boolean'Wide_Wide_Value
          (VSS.Strings.Conversions.To_Wide_Wide_String
             (VSS.Application.Arguments.Element (2)));
      Encoded   : constant VSS.Stream_Element_Vectors.Stream_Element_Vector :=
        Load (VSS.Application.Arguments.Element (3));
      Decoded   : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String
             (Load (VSS.Application.Arguments.Element (4))));

   begin
      Test_Support.Assert (not Encoding.Is_Empty);
      Test_Support.Assert (not Encoded.Is_Empty);
      Test_Support.Assert (not Decoded.Is_Empty);

      Run_Decoder_Block_Mode (Encoding, Has_Error, Encoded, Decoded);
      Run_Decoder_Incremental_Mode (Encoding, Has_Error, Encoded, Decoded);
   end;
end Test_String_Decoder;
