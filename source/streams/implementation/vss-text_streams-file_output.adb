--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;
with Interfaces.C;

with VSS.Stream_Element_Vectors.Internals;
with VSS.Strings.Conversions;

package body VSS.Text_Streams.File_Output is

   use type Interfaces.C_Streams.FILEs;

   C_W_Mode : constant Interfaces.C.char_array := Interfaces.C.To_C ("w");

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out File_Output_Text_Stream'Class) is
      Dummy : Interfaces.C_Streams.int;

   begin
      if Self.Stream /= Interfaces.C_Streams.NULL_Stream then
         Dummy := Interfaces.C_Streams.fclose (Self.Stream);
      end if;

      Self.Stream := Interfaces.C_Streams.NULL_Stream;
      Self.Encoder.Reset_State;
      Self.Error.Clear;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self : in out File_Output_Text_Stream'Class;
      Name : VSS.Strings.Virtual_String'Class)
   is
      C_Name : constant Interfaces.C.char_array :=
        Interfaces.C.To_C (VSS.Strings.Conversions.To_UTF_8_String (Name));

   begin
      if Self.Stream /= Interfaces.C_Streams.NULL_Stream then
         Self.Close;
      end if;

      Self.Error.Clear;

      Self.Stream :=
        Interfaces.C_Streams.fopen
          (C_Name (C_Name'First)'Address,
           C_W_Mode (C_W_Mode'First)'Address);

      if Self.Stream = Interfaces.C_Streams.NULL_Stream then
         Self.Error := "Unable to create file";

      elsif not Self.Encoder.Is_Valid then
         Self.Encoder.Initialize ("utf-8");
      end if;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self     : in out File_Output_Text_Stream'Class;
      Name     : VSS.Strings.Virtual_String'Class;
      Encoding : VSS.Strings.Virtual_String) is
   begin
      if Self.Stream /= Interfaces.C_Streams.NULL_Stream then
         Self.Close;
      end if;

      Self.Error.Clear;

      Self.Set_Encoding (Encoding);

      if Self.Error.Is_Empty then
         Self.Create (Name);
      end if;
   end Create;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : File_Output_Text_Stream) return VSS.Strings.Virtual_String is
   begin
      return Self.Error;
   end Error_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out File_Output_Text_Stream) is
   begin
      Self.Close;
   end Finalize;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error
     (Self : File_Output_Text_Stream) return Boolean is
   begin
      return not Self.Error.Is_Empty;
   end Has_Error;

   ---------
   -- Put --
   ---------

   procedure Put
     (Self    : in out File_Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      use type Interfaces.C_Streams.size_t;

      Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Length : Ada.Streams.Stream_Element_Count;
      Data   :
        VSS.Stream_Element_Vectors.Internals.Stream_Element_Array_Access;
      Size   : Interfaces.C_Streams.size_t;

   begin
      if Self.Stream = Interfaces.C_Streams.NULL_Stream then
         Self.Error := "File is not open";
         Success    := False;

         return;
      end if;

      if not Self.Error.Is_Empty then
         Success := False;

         return;
      end if;

      Self.Encoder.Encode (Item, Buffer);

      if not Buffer.Is_Empty then
         VSS.Stream_Element_Vectors.Internals.Data_Constant_Access
           (Buffer, Length, Data);

         Size :=
           Interfaces.C_Streams.fwrite
             (Data (Data'First)'Address,
              1,
              Interfaces.C_Streams.size_t (Length),
              Self.Stream);

         if Size /= Interfaces.C_Streams.size_t (Length) then
            Self.Error := "File IO error";
            Success    := False;
         end if;
      end if;
   end Put;

   ------------------
   -- Set_Encoding --
   ------------------

   procedure Set_Encoding
     (Self     : in out File_Output_Text_Stream'Class;
      Encoding : VSS.Strings.Virtual_String)
   is
      use all type VSS.Strings.Converters.Converter_Flag;

   begin
      if Self.Stream = Interfaces.C_Streams.NULL_Stream then
         Self.Encoder.Initialize
           (Encoding,
            (Stateless     => False,
             Stop_On_Error => False,
             Process_BOM   => False));

         if not Self.Encoder.Is_Valid then
            Self.Error := "Unsupported encoding";

         else
            Self.Error.Clear;
         end if;
      end if;
   end Set_Encoding;
end VSS.Text_Streams.File_Output;
