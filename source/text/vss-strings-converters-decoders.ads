------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
--  This package provides string data decoder from external encoding to
--  Virtual_String.

private with Ada.Finalization;

with VSS.Stream_Element_Buffers;
private with VSS.Unicode;

package VSS.Strings.Converters.Decoders is

   type Virtual_String_Decoder is tagged limited private;

   procedure Initialize
     (Self     : in out Virtual_String_Decoder'Class;
      Encoding : VSS.Strings.Virtual_String;
      Flags    : Converter_Flags := Default_Converter_Flags);

   function Is_Valid (Self : Virtual_String_Decoder'Class) return Boolean;

   function Has_Error (Self : Virtual_String_Decoder'Class) return Boolean;

   function Error_Message
     (Self : Virtual_String_Decoder'Class) return VSS.Strings.Virtual_String;

   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : VSS.Stream_Element_Buffers.Stream_Element_Buffer)
      return VSS.Strings.Virtual_String;

   procedure Reset_State (Self : in out Virtual_String_Decoder'Class);

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
