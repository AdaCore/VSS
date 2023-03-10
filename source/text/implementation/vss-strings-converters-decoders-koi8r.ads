--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  KOI8-R decoder.

private package VSS.Strings.Converters.Decoders.KOI8R is

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access;
   --  Create KOI8R_Decoder and return it

private

   type KOI8R_Decoder is new Abstract_Decoder with null record;

   overriding procedure Decode
     (Self        : in out KOI8R_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data);

   overriding function Has_Error
     (Self : KOI8R_Decoder) return Boolean is (False);

   overriding function Error_Message
     (Self : KOI8R_Decoder)
      return VSS.Strings.Virtual_String is (VSS.Strings.Empty_Virtual_String);

   overriding procedure Reset_State (Self : in out KOI8R_Decoder) is null;

end VSS.Strings.Converters.Decoders.KOI8R;
