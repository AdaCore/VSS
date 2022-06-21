--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  UTF-8 decoder. Implementation is conformant with W3C/Unicode requirements
--  to process ill-formed sequences with substitutions of U+FFFD REPLACEMENT
--  CHARACTER.

private package VSS.Strings.Converters.Decoders.UTF8 is

   type UTF8_Decoder is new Abstract_Decoder with private;

private

   type UTF8_Decoder is new Abstract_Decoder with record
      Flags    : Converter_Flags;
      Code     : VSS.Unicode.Code_Point;
      Needed   : VSS.Unicode.UTF8_Code_Unit_Count;
      Seen     : VSS.Unicode.UTF8_Code_Unit_Count;
      Lower    : Ada.Streams.Stream_Element;
      Upper    : Ada.Streams.Stream_Element;
      Error    : Boolean;
      Skip_BOM : Boolean;
   end record;

   overriding procedure Initialize
     (Self  : in out UTF8_Decoder;
      Flags : Converter_Flags);

   overriding procedure Decode
     (Self   : in out UTF8_Decoder;
      Source : Ada.Streams.Stream_Element_Array;
      Target : out VSS.Implementation.Strings.String_Data);

   overriding function Has_Error (Self : UTF8_Decoder) return Boolean;

   overriding function Error_Message
     (Self : UTF8_Decoder) return VSS.Strings.Virtual_String;

   overriding procedure Reset_State (Self : in out UTF8_Decoder);

end VSS.Strings.Converters.Decoders.UTF8;
