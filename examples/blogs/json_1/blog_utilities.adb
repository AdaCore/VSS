
with VSS.Strings.Converters.Decoders;
with VSS.Strings.Converters.Encoders;
with VSS.Strings.Conversions;

package body Blog_Utilities is

   ------------------------------
   -- To_Stream_Element_Vector --
   ------------------------------

   function Encode
     (Text : Wide_Wide_String)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector
   is
      Encoder : VSS.Strings.Converters.Encoders.Virtual_String_Encoder;

   begin
      Encoder.Initialize
        ("utf-8", (VSS.Strings.Converters.Stateless => True, others => False));

      return Encoder.Encode (VSS.Strings.To_Virtual_String (Text));
   end Encode;

   -------------------------
   -- To_Wide_Wide_String --
   -------------------------

   function Decode
     (Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return Wide_Wide_String
   is
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;

   begin
      Decoder.Initialize
        ("utf-8", (VSS.Strings.Converters.Stateless => True, others => False));

      return
        VSS.Strings.Conversions.To_Wide_Wide_String (Decoder.Decode (Buffer));
   end Decode;

end Blog_Utilities;
