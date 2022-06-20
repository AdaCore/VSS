--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.UTF8_Encoding;

package body VSS.Strings.Converters.Encoders.UTF8 is

   ------------
   -- Encode --
   ------------

   overriding procedure Encode
     (Self   : in out UTF8_Encoder;
      Source : VSS.Unicode.Code_Point;
      Target : in out VSS.Stream_Element_Vectors.Stream_Element_Vector'Class)
   is
      Size    : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      Encoded :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
          (VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length'Range);
      Buffer  : Ada.Streams.Stream_Element_Array
                  (Ada.Streams.Stream_Element_Offset (Encoded'First)
                     .. Ada.Streams.Stream_Element_Offset (Encoded'Last))
        with Address => Encoded'Address;

   begin
      if not Self.BOM_Written then
         Self.BOM_Written := True;
         Self.Encode (Zero_Width_No_Break_Space_Character, Target);
      end if;

      VSS.Implementation.UTF8_Encoding.Encode
        (Source,
         Size,
         Encoded (1),
         Encoded (2),
         Encoded (3),
         Encoded (4));

      for J in Buffer'First .. Ada.Streams.Stream_Element_Offset (Size) loop
         Target.Append (Buffer (J));
      end loop;
   end Encode;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : UTF8_Encoder) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error (Self : UTF8_Encoder) return Boolean is
   begin
      return False;
   end Has_Error;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self  : in out UTF8_Encoder;
      Flags : Converter_Flags) is
   begin
      Self.Flags := Flags;
      Self.Reset_State;
   end Initialize;

end VSS.Strings.Converters.Encoders.UTF8;
