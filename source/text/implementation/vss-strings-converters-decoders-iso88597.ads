--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  ISO-8859-7 decoder.

private package VSS.Strings.Converters.Decoders.ISO88597 is

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access;
   --  Create ISO88597_Decoder and return it

private

   type ISO88597_Decoder is new Abstract_Decoder with record
      Flags : Converter_Flags;
      Error : Boolean;
   end record;

   overriding procedure Decode
     (Self        : in out ISO88597_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data);

   overriding function Has_Error (Self : ISO88597_Decoder) return Boolean;

   overriding function Error_Message
     (Self : ISO88597_Decoder) return VSS.Strings.Virtual_String;

   overriding procedure Reset_State (Self : in out ISO88597_Decoder);

end VSS.Strings.Converters.Decoders.ISO88597;
