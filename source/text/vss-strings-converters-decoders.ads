--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides string data decoder from external encoding to
--  Virtual_String.

private with Ada.Finalization;
with Ada.Streams;

with VSS.Stream_Element_Vectors;
private with VSS.Unicode;

package VSS.Strings.Converters.Decoders is

   type Virtual_String_Decoder is tagged limited private;

   procedure Initialize
     (Self     : in out Virtual_String_Decoder'Class;
      Encoding : VSS.Strings.Virtual_String;
      Flags    : Converter_Flags := Default_Converter_Flags);
   --  Initialize decoder to decode text data in given encoding. Is_Valid
   --  return True when decoder has been initialized successfully.

   function Is_Valid (Self : Virtual_String_Decoder'Class) return Boolean;
   --  Return True when decoder is initialized successfully.

   function Has_Error (Self : Virtual_String_Decoder'Class) return Boolean;
   --  Return True when some error has been found during decoding.

   function Error_Message
     (Self : Virtual_String_Decoder'Class) return VSS.Strings.Virtual_String;
   --  Return error message for latest detected error.

   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return VSS.Strings.Virtual_String;
   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : Ada.Streams.Stream_Element_Array)
      return VSS.Strings.Virtual_String;
   --  Decode Data and return result. When Stateless flag was set to False,
   --  it returns only fully decoded portion of the data, and save incomplete
   --  data to be decoded with next call of Decode. When Stateless flag was
   --  set to True, incomplete data is not allowed, it will be returned as
   --  decoding error.

   procedure Reset_State (Self : in out Virtual_String_Decoder'Class);
   --  Reset state of the decoder to initial (after call of Initialize).
   --  All accumulated incomplete data will be lost.

private

   Replacement_Character : constant VSS.Unicode.Code_Point := 16#FFFD#;

   type Abstract_Decoder is abstract tagged limited null record;

   type Decoder_Access is access all Abstract_Decoder'Class;

   not overriding procedure Initialize
     (Self  : in out Abstract_Decoder;
      Flags : Converter_Flags) is abstract;

   not overriding procedure Decode
     (Self   : in out Abstract_Decoder;
      Source : Ada.Streams.Stream_Element_Array;
      Target : out VSS.Implementation.Strings.String_Data) is abstract;

   not overriding function Has_Error
     (Self : Abstract_Decoder) return Boolean is abstract;

   not overriding function Error_Message
     (Self : Abstract_Decoder) return VSS.Strings.Virtual_String is abstract;

   not overriding procedure Reset_State
     (Self : in out Abstract_Decoder) is abstract;

   type Virtual_String_Decoder is
     new Ada.Finalization.Limited_Controlled with record
      Decoder : Decoder_Access;
   end record;

   overriding procedure Finalize (Self : in out Virtual_String_Decoder);

end VSS.Strings.Converters.Decoders;
