--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  UTF-8 encoder.

private package VSS.Strings.Converters.Encoders.UTF8 is

   type UTF8_Encoder is new Abstract_Encoder with private;

private

   type UTF8_Encoder is new Abstract_Encoder with null record;

   overriding procedure Initialize
     (Self  : in out UTF8_Encoder;
      Flags : Converter_Flags);

   overriding procedure Encode
     (Self   : in out UTF8_Encoder;
      Source : VSS.Unicode.Code_Point;
      Target : in out VSS.Stream_Element_Vectors.Stream_Element_Vector'Class);

   overriding function Has_Error (Self : UTF8_Encoder) return Boolean;

   overriding function Error_Message
     (Self : UTF8_Encoder) return VSS.Strings.Virtual_String;

end VSS.Strings.Converters.Encoders.UTF8;
