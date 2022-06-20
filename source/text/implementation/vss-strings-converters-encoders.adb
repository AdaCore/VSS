--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

with VSS.Implementation.String_Handlers;
with VSS.Strings.Converters.Encoders.UTF8;

package body VSS.Strings.Converters.Encoders is

   procedure Free is
     new Ada.Unchecked_Deallocation (Abstract_Encoder'Class, Encoder_Access);

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Self   : in out Virtual_String_Encoder'Class;
      Item   : VSS.Characters.Virtual_Character;
      Buffer : in out VSS.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      if Self.Encoder /= null then
         Self.Encoder.Encode
           (VSS.Characters.Virtual_Character'Pos (Item), Buffer);
      end if;
   end Encode;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Self   : in out Virtual_String_Encoder'Class;
      Item   : VSS.Strings.Virtual_String'Class;
      Buffer : in out VSS.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      if not Item.Is_Empty and Self.Encoder /= null then
         Self.Encoder.Encode (Item.Data, Buffer);
      end if;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : in out Virtual_String_Encoder'Class;
      Item : VSS.Characters.Virtual_Character)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector is
   begin
      return Result : VSS.Stream_Element_Vectors.Stream_Element_Vector do
         if Self.Encoder /= null then
            Self.Encoder.Encode
              (VSS.Characters.Virtual_Character'Pos (Item), Result);
         end if;
      end return;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : in out Virtual_String_Encoder'Class;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Stream_Element_Vectors.Stream_Element_Vector is
   begin
      return Result : VSS.Stream_Element_Vectors.Stream_Element_Vector do
         if Self.Encoder /= null then
            Self.Encoder.Encode (Item.Data, Result);
         end if;
      end return;
   end Encode;

   ------------
   -- Encode --
   ------------

   not overriding procedure Encode
     (Self   : in out Abstract_Encoder;
      Source : VSS.Implementation.Strings.String_Data;
      Target : in out VSS.Stream_Element_Vectors.Stream_Element_Vector'Class)
   is
      Handler  :
        constant not null VSS.Implementation.Strings.String_Handler_Access
          := VSS.Implementation.Strings.Handler (Source);
      Position : VSS.Implementation.Strings.Cursor;

   begin
      if not Self.BOM_Written then
         Self.BOM_Written := True;
         Abstract_Encoder'Class (Self).Encode
           (Zero_Width_No_Break_Space_Character, Target);
      end if;

      Handler.Before_First_Character (Source, Position);

      while Handler.Forward (Source, Position)
        and not Abstract_Encoder'Class (Self).Has_Error
      loop
         Abstract_Encoder'Class (Self).Encode
           (Handler.Element (Source, Position), Target);
      end loop;
   end Encode;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : Virtual_String_Encoder'Class) return VSS.Strings.Virtual_String is
   begin
      if Self.Encoder /= null then
         return Self.Encoder.Error_Message;

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Error_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String_Encoder) is
   begin
      Free (Self.Encoder);
   end Finalize;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Self : Virtual_String_Encoder'Class) return Boolean is
   begin
      if Self.Encoder /= null then
         return Self.Encoder.Has_Error;

      else
         return False;
      end if;
   end Has_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Virtual_String_Encoder'Class;
      Encoding : VSS.Strings.Virtual_String;
      Flags    : Converter_Flags := Default_Converter_Flags) is
   begin
      Free (Self.Encoder);

      if Encoding = "utf-8" then
         Self.Encoder :=
           new VSS.Strings.Converters.Encoders.UTF8.UTF8_Encoder;
         Self.Encoder.Initialize (Flags);
      end if;
   end Initialize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Virtual_String_Encoder'Class) return Boolean is
   begin
      return Self.Encoder /= null;
   end Is_Valid;

   -----------------
   -- Reset_State --
   -----------------

   not overriding procedure Reset_State (Self : in out Abstract_Encoder) is
   begin
      Self.BOM_Written := not Self.Flags (Process_BOM);
   end Reset_State;

end VSS.Strings.Converters.Encoders;
