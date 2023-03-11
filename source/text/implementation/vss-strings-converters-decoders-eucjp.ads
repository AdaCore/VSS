--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  EUC-JP decoder.

private package VSS.Strings.Converters.Decoders.EUCJP is

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access;
   --  Create EUCJP_Decoder and return it

private

   type EUCJP_Decoder is new Abstract_Decoder with record
      Flags   : Converter_Flags;
      JIS0212 : Boolean;
      Lead    : Ada.Streams.Stream_Element;
      Error   : Boolean;
   end record;

   overriding procedure Decode
     (Self        : in out EUCJP_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data);

   overriding function Has_Error (Self : EUCJP_Decoder) return Boolean;

   overriding function Error_Message
     (Self : EUCJP_Decoder) return VSS.Strings.Virtual_String;

   overriding procedure Reset_State (Self : in out EUCJP_Decoder);

end VSS.Strings.Converters.Decoders.EUCJP;
