--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;
with Interfaces.C;

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

package body VSS.Text_Streams.File_Input is

   use type Interfaces.C_Streams.FILEs;

   C_R_Mode : constant Interfaces.C.char_array := Interfaces.C.To_C ("r");

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out File_Input_Text_Stream'Class) is
      Dummy : Interfaces.C_Streams.int;

   begin
      if Self.Stream /= Interfaces.C_Streams.NULL_Stream then
         Dummy := Interfaces.C_Streams.fclose (Self.Stream);
      end if;

      Self.Stream := Interfaces.C_Streams.NULL_Stream;
      Self.Decoder.Reset_State;
      Self.Buffer.Clear;
      Self.Error.Clear;
   end Close;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : File_Input_Text_Stream) return VSS.Strings.Virtual_String is
   begin
      return Self.Error;
   end Error_Message;

   ---------
   -- Get --
   ---------

   overriding procedure Get
     (Self    : in out File_Input_Text_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean) is
   begin
      if Self.Buffer.Is_Empty then
         if Self.Stream = Interfaces.C_Streams.NULL_Stream then
            Self.Error := "File is not open";
            Item       := VSS.Characters.Latin.Nul;
            Success    := False;

            return;
         end if;

         declare
            use type Ada.Streams.Stream_Element_Offset;
            use type Interfaces.C_Streams.size_t;

            Data : Ada.Streams.Stream_Element_Array (1 .. 128);
            Size : Interfaces.C_Streams.size_t;

         begin
            Size :=
              Interfaces.C_Streams.fread
                (Data (Data'First)'Address, 0, 1, Data'Length, Self.Stream);

            if Size /= 0 then
               Self.Buffer :=
                 Self.Decoder.Decode
                   (Data
                      (Data'First
                         .. Data'First
                              + Ada.Streams.Stream_Element_Offset (Size) - 1));

            elsif Interfaces.C_Streams.ferror (Self.Stream) /= 0 then
               Self.Error := "File IO error";
            end if;
         end;
      end if;

      if Self.Buffer.Is_Empty then
         Item    := VSS.Characters.Latin.Nul;
         Success := False;

      else
         declare
            First_Character : constant
              VSS.Strings.Character_Iterators.Character_Iterator :=
                Self.Buffer.At_First_Character;

         begin
            Item := First_Character.Element;
            Self.Buffer := Self.Buffer.Tail_After (First_Character);
            Success := True;
         end;
      end if;
   end Get;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error
     (Self : File_Input_Text_Stream) return Boolean is
   begin
      return not Self.Error.Is_Empty;
   end Has_Error;

   --------------------
   -- Is_End_Of_Data --
   --------------------

   overriding function Is_End_Of_Data
     (Self : File_Input_Text_Stream) return Boolean is
   begin
      return False;
   end Is_End_Of_Data;

   ----------------------
   -- Is_End_Of_Stream --
   ----------------------

   overriding function Is_End_Of_Stream
     (Self : File_Input_Text_Stream) return Boolean is
   begin
      return
        Self.Stream = Interfaces.C_Streams.NULL_Stream
          or else
            (Self.Buffer.Is_Empty
               and then Interfaces.C_Streams.feof (Self.Stream) /= 0);
   end Is_End_Of_Stream;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self : in out File_Input_Text_Stream'Class;
      Name : VSS.Strings.Virtual_String)
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
           C_R_Mode (C_R_Mode'First)'Address);

      if Self.Stream = Interfaces.C_Streams.NULL_Stream then
         Self.Error := "Unable to open file";

      elsif not Self.Decoder.Is_Valid then
         Self.Decoder.Initialize ("utf-8");
      end if;
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self     : in out File_Input_Text_Stream'Class;
      Name     : VSS.Strings.Virtual_String;
      Encoding : VSS.Strings.Virtual_String) is
   begin
      if Self.Stream /= Interfaces.C_Streams.NULL_Stream then
         Self.Close;
      end if;

      Self.Error.Clear;

      Self.Set_Encoding (Encoding);

      if Self.Error.Is_Empty then
         Self.Open (Name);
      end if;
   end Open;

   ------------------
   -- Set_Encoding --
   ------------------

   procedure Set_Encoding
     (Self     : in out File_Input_Text_Stream'Class;
      Encoding : VSS.Strings.Virtual_String)
   is
      use all type VSS.Strings.Converters.Converter_Flag;

   begin
      if Self.Stream = Interfaces.C_Streams.NULL_Stream then
         Self.Decoder.Initialize
           (Encoding,
            (Stateless     => False,
             Stop_On_Error => False,
             Process_BOM   => False));

         if not Self.Decoder.Is_Valid then
            Self.Error := "Unsupported encoding";

         else
            Self.Error.Clear;
         end if;
      end if;
   end Set_Encoding;

end VSS.Text_Streams.File_Input;
