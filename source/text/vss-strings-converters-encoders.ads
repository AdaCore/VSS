--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides string data encoder from Virtual_String to external
--  encoding.

private with Ada.Finalization;

with VSS.Stream_Element_Vectors;
private with VSS.Unicode;

package VSS.Strings.Converters.Encoders is

   type Virtual_String_Encoder is tagged limited private;

   procedure Initialize
     (Self     : in out Virtual_String_Encoder'Class;
      Encoding : VSS.Strings.Virtual_String;
      Flags    : Converter_Flags := Default_Converter_Flags);
   --  Initialize text encoder to encode text data in given encoding. Is_Valid
   --  return True when encoder has been initialized successfully.

   function Is_Valid (Self : Virtual_String_Encoder'Class) return Boolean;
   --  Return True when encoder is initialized successfully.

   function Has_Error (Self : Virtual_String_Encoder'Class) return Boolean;
   --  Return True when some error has been found during encoding.

   function Error_Message
     (Self : Virtual_String_Encoder'Class) return VSS.Strings.Virtual_String;
   --  Return error message for latest detected error.

   procedure Encode
     (Self   : in out Virtual_String_Encoder'Class;
      Item   : VSS.Characters.Virtual_Character;
      Buffer : in out VSS.Stream_Element_Vectors.Stream_Element_Vector);
   procedure Encode
     (Self   : in out Virtual_String_Encoder'Class;
      Item   : VSS.Strings.Virtual_String'Class;
      Buffer : in out VSS.Stream_Element_Vectors.Stream_Element_Vector);
   --  Encode Item and append result to given Buffer. When Stateless flag was
   --  set to False, it returns only fully decoded portion of the data, and
   --  save incomplete data to be encoded with next call of Encode. When
   --  Stateless flag was set to True, incomplete data is not allowed, it will
   --  be returned as encoding error.

   function Encode
     (Self : in out Virtual_String_Encoder'Class;
      Item : VSS.Characters.Virtual_Character)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector;
   function Encode
     (Self : in out Virtual_String_Encoder'Class;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector;
   --  Encode Item and return result. When Stateless flag was set to False,
   --  it returns only fully decoded portion of the data, and save incomplete
   --  data to be encoded with next call of Encode. When Stateless flag was
   --  set to True, incomplete data is not allowed, it will be returned as
   --  encoding error.

private

   Zero_Width_No_Break_Space_Character :
     constant VSS.Unicode.Code_Point := 16#FEFF#;

   type Abstract_Encoder is abstract tagged limited record
      Flags       : Converter_Flags;
      BOM_Written : Boolean;
   end record;

   type Encoder_Access is access all Abstract_Encoder'Class;

   not overriding procedure Initialize
     (Self  : in out Abstract_Encoder;
      Flags : Converter_Flags) is abstract;

   not overriding procedure Encode
     (Self   : in out Abstract_Encoder;
      Source : VSS.Unicode.Code_Point;
      Target : in out VSS.Stream_Element_Vectors.Stream_Element_Vector'Class)
        is abstract;

   not overriding procedure Encode
     (Self   : in out Abstract_Encoder;
      Source : VSS.Implementation.Strings.String_Data;
      Target : in out VSS.Stream_Element_Vectors.Stream_Element_Vector'Class);

   not overriding function Has_Error
     (Self : Abstract_Encoder) return Boolean is abstract;

   not overriding function Error_Message
     (Self : Abstract_Encoder) return VSS.Strings.Virtual_String is abstract;

   not overriding procedure Reset_State (Self : in out Abstract_Encoder);

   type Virtual_String_Encoder is
     new Ada.Finalization.Limited_Controlled with record
      Encoder : Encoder_Access;
   end record;

   overriding procedure Finalize (Self : in out Virtual_String_Encoder);

end VSS.Strings.Converters.Encoders;
