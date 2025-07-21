--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  ISO-8859-9 decoder.

private package VSS.Strings.Converters.Decoders.ISO88599 is

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access;
   --  Create ISO88599_Decoder and return it

private

   type ISO88599_Decoder is new Abstract_Decoder with null record;

   overriding procedure Decode
     (Self        : in out ISO88599_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Text        : out VSS.Implementation.UTF8_Strings.UTF8_String_Data);

   overriding function Has_Error
     (Self : ISO88599_Decoder) return Boolean is (False);

   overriding function Error_Message
     (Self : ISO88599_Decoder)
      return VSS.Strings.Virtual_String is (VSS.Strings.Empty_Virtual_String);

   overriding procedure Reset_State (Self : in out ISO88599_Decoder) is null;

end VSS.Strings.Converters.Decoders.ISO88599;
