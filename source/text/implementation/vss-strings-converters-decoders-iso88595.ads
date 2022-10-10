--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  ISO-8859-5 decoder.

private package VSS.Strings.Converters.Decoders.ISO88595 is

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access;
   --  Create ISO88595_Decoder and return it

private

   type ISO88595_Decoder is new Abstract_Decoder with null record;

   overriding procedure Decode
     (Self        : in out ISO88595_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data);

   overriding function Has_Error
     (Self : ISO88595_Decoder) return Boolean is (False);

   overriding function Error_Message
     (Self : ISO88595_Decoder)
      return VSS.Strings.Virtual_String is (VSS.Strings.Empty_Virtual_String);

   overriding procedure Reset_State (Self : in out ISO88595_Decoder) is null;

end VSS.Strings.Converters.Decoders.ISO88595;
