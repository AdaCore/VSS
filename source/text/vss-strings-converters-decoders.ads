--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
     (Self        : in out Virtual_String_Decoder'Class;
      Data_Chunk  : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      End_Of_Data : Boolean := True)
      return VSS.Strings.Virtual_String;
   function Decode
     (Self        : in out Virtual_String_Decoder'Class;
      Data_Chunk  : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean := True)
      return VSS.Strings.Virtual_String;
   --  Decode Data and return result. When Stateless flag was set to False,
   --  and End_Of_Data parameter is False, it returns only fully decoded
   --  portion of the data, and save incomplete data to be decoded with next
   --  call of Decode. When Stateless flag was set to True, or End_Of_Data is
   --  True, incomplete data is not allowed, it will be returned as decoding
   --  error.

   procedure Reset_State (Self : in out Virtual_String_Decoder'Class);
   --  Reset state of the decoder to initial (after call of Initialize).
   --  All accumulated incomplete data will be lost.

private

   Replacement_Character : constant VSS.Unicode.Code_Point := 16#FFFD#;

   subtype ASCII_Byte_Range is
     Ada.Streams.Stream_Element range 16#00# .. 16#7F#;

   type Abstract_Decoder is abstract tagged limited null record;

   type Decoder_Access is access all Abstract_Decoder'Class;

   procedure Decode
     (Self        : in out Abstract_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Text        : out VSS.Implementation.UTF8_Strings.UTF8_String_Data)
        is abstract;
   --  Decode Source chunk of the data and append result to Target.

   function Has_Error
     (Self : Abstract_Decoder) return Boolean is abstract;

   function Error_Message
     (Self : Abstract_Decoder) return VSS.Strings.Virtual_String is abstract;

   procedure Reset_State
     (Self : in out Abstract_Decoder) is abstract;

   type Virtual_String_Decoder is
     new Ada.Finalization.Limited_Controlled with record
      Decoder : Decoder_Access;
   end record;

   overriding procedure Finalize (Self : in out Virtual_String_Decoder);

end VSS.Strings.Converters.Decoders;
