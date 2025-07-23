--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Streams;
with Interfaces.C;

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;

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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out File_Input_Text_Stream) is
   begin
      Self.Close;
   end Finalize;

   ---------
   -- Get --
   ---------

   overriding procedure Get
     (Self    : in out File_Input_Text_Stream;
      Item    : out VSS.Characters.Virtual_Character'Base;
      Success : in out Boolean) is
   begin
      Self.Populate_Buffer (Success);

      if not Success then
         Item       := VSS.Characters.Virtual_Character'Base'Last;
         return;
      end if;

      if Self.Buffer.Is_Empty then
         Item    := VSS.Characters.Virtual_Character'Base'Last;
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

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
      (Self        : in out File_Input_Text_Stream'Class;
       Line        : out VSS.Strings.Virtual_String'Class;
       Success     : out Boolean;
       Terminators : VSS.Strings.Line_Terminator_Set :=
         VSS.Strings.New_Line_Function;
       Keep_Terminator : Boolean := True)
   is

      use VSS.Characters.Latin;
      use VSS.Strings;

      use type VSS.Characters.Virtual_Character;

      procedure Terminate_Line;
      procedure Read_Char;

      Char : VSS.Characters.Virtual_Character;
      Line_Staging : Virtual_String'Class := "";

      procedure Read_Char is
      begin
         Get (Self, Char, Success);
         if not Success then
            return;
         end if;

         --  Go ahead and refill the buffer in case we need lookahead. Later
         --  on, this will disambiguate the cases of an empty buffer or EOF.
         if Self.Buffer.Is_Empty then
            Self.Populate_Buffer (Success);
            if not Success then
               return;
            end if;
         end if;

         Line_Staging := Line_Staging & Char; -- GNAT complains about @ here.
      end Read_Char;

      procedure Terminate_Line
      is
         LF_Prefix : constant VSS.Strings.Virtual_String := "" & Line_Feed;
      begin

         --  Only one case can be ambiguous, and that's when both CR ∈
         --  Terminators and CRLF ∈ Terminators. Because we only read one
         --  character at a time, we'll see Char = CR and at least one byte
         --  avaliable to read when facing this case, and we'll go on to
         --  check lookahead.
         if Char /= Carriage_Return or else
           Self.Buffer.Is_Empty or else
           not Terminators (CR) or else
           not Terminators (CRLF)
         then
            return;
         end if;

         if Self.Buffer.Starts_With (LF_Prefix) then
            Read_Char;
         end if;

      end Terminate_Line;

   begin

      --  We read the line one character at a time, noting and resolving the
      --  ambiguity of CRLF and CR both being potentially acceptable line
      --  endings by looking ahead when CR portends to end a line on its own.

      loop
         Read_Char;
         exit when not Success;

         if Line_Staging.Ends_With (Terminators) then
            Terminate_Line;
            if Keep_Terminator then
               Line := Line_Staging;
            end if;

            return;
         end if;

         --   Only always copy the additions over to the output when we've
         --   verified that we haven't just read in a terminator.
         Line := Line_Staging;

         --  Check whether we're at the end of the file. If this is the case,
         --  then the buffer will be empty via Read_Char.
         if Self.Buffer.Is_Empty then
            return;
         end if;

      end loop;

   end Get_Line;

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
         declare
            Template : constant
              VSS.Strings.Templates.Virtual_String_Template :=
                "Unable to open file '{}'";

         begin
            Self.Error :=
              Template.Format
                (VSS.Strings.Formatters.Strings.Image (Name));
         end;

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

   ---------------------
   -- Populate_Buffer --
   ---------------------

   procedure Populate_Buffer
      (Self    : in out File_Input_Text_Stream;
       Success : out Boolean) is
   begin
      if Self.Buffer.Is_Empty then
         if Self.Stream = Interfaces.C_Streams.NULL_Stream then
            Self.Error := "File is not open";
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
               --  Some data has been read, decode it.

               Self.Buffer :=
                 Self.Decoder.Decode
                   (Data
                      (Data'First
                         .. Data'First
                              + Ada.Streams.Stream_Element_Offset (Size) - 1),
                    False);

            elsif Interfaces.C_Streams.feof (Self.Stream) /= 0 then
               --  End of file has been reached, let decoder know that no more
               --  data available. Decoder will return REPLACEMENT CHARACTER
               --  if some data has beed accumulated but can't be decoded.

               Self.Buffer :=
                 Self.Decoder.Decode
                   (Data (Data'First .. Data'First - 1), True);

            elsif Interfaces.C_Streams.ferror (Self.Stream) /= 0 then
               Self.Error := "File IO error";
            end if;
         end;
      end if;

      Success := True;
   end Populate_Buffer;

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
            [Stateless     => False,
             Stop_On_Error => False,
             Process_BOM   => False]);

         if not Self.Decoder.Is_Valid then
            Self.Error := "Unsupported encoding";

         else
            Self.Error.Clear;
         end if;
      end if;
   end Set_Encoding;

end VSS.Text_Streams.File_Input;
