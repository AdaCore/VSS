--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  GB18030 decoder.

private package VSS.Strings.Converters.Decoders.GB18030 is

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access;
   --  Create GB18030_Decoder and return it

private

   type GB18030_Decoder is new Abstract_Decoder with record
      Flags  : Converter_Flags;
      First  : Ada.Streams.Stream_Element;
      Second : Ada.Streams.Stream_Element;
      Third  : Ada.Streams.Stream_Element;
      Error  : Boolean;
   end record;

   overriding procedure Decode
     (Self        : in out GB18030_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data);

   overriding function Has_Error (Self : GB18030_Decoder) return Boolean;

   overriding function Error_Message
     (Self : GB18030_Decoder) return VSS.Strings.Virtual_String;

   overriding procedure Reset_State (Self : in out GB18030_Decoder);

end VSS.Strings.Converters.Decoders.GB18030;
