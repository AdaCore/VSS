--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Configuration;

package body VSS.Strings.Converters.Decoders.UTF8 is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self   : in out UTF8_Decoder;
      Source : Ada.Streams.Stream_Element_Array;
      Target : out VSS.Implementation.Strings.String_Data)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      use type VSS.Unicode.Code_Point;
      use type VSS.Unicode.UTF8_Code_Unit_Count;

      Index  : Ada.Streams.Stream_Element_Offset := Source'First;
      Code   : VSS.Unicode.Code_Point            := Self.Code;
      Needed : VSS.Unicode.UTF8_Code_Unit_Count  := Self.Needed;
      Seen   : VSS.Unicode.UTF8_Code_Unit_Count  := Self.Seen;
      Lower  : Ada.Streams.Stream_Element        := Self.Lower;
      Upper  : Ada.Streams.Stream_Element        := Self.Upper;
      Byte   : Ada.Streams.Stream_Element;
      Offset : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      if (Self.Error and Self.Flags (Stop_On_Error))
        or else Source'Last < Source'First
      then
         --  Error was encountered in "stop on error" mode or source data is
         --  empty: return "null" string.

         Target := VSS.Implementation.Strings.Null_String_Data;

      else
         VSS.Implementation.String_Configuration.In_Place_Handler.Initialize
           (Target);

         loop
            exit when Index > Source'Last;

            Byte := Source (Index);

            if Needed = 0 then
               case Byte is
                  when 16#00# .. 16#7F# =>
                     VSS.Implementation.Strings.Handler (Target).Append
                       (Target,
                        VSS.Unicode.Code_Point (Byte and 16#7F#),
                        Offset);

                  when 16#C2# .. 16#DF# =>
                     Code := VSS.Unicode.Code_Point (Byte and 16#1F#);
                     Needed := 1;

                  when 16#E0# .. 16#EF# =>
                     Code := VSS.Unicode.Code_Point (Byte and 16#0F#);
                     Needed := 2;

                     if Byte = 16#E0# then
                        Lower := 16#A0#;

                     elsif Byte = 16#ED# then
                        Upper := 16#9F#;
                     end if;

                  when 16#F0# .. 16#F4# =>
                     Code := VSS.Unicode.Code_Point (Byte and 16#07#);
                     Needed := 3;

                     if Byte = 16#F0# then
                        Lower := 16#90#;

                     elsif Byte = 16#F4# then
                        Upper := 16#8F#;
                     end if;

                  when others =>
                     Self.Error := True;

                     if Self.Flags (Stop_On_Error) then
                        exit;

                     else
                        VSS.Implementation.Strings.Handler (Target).Append
                          (Target, Replacement_Character, Offset);
                     end if;
               end case;

            elsif Byte in Lower .. Upper then
               Lower := 16#80#;
               Upper := 16#BF#;
               Code  :=
                 Code * 16#40# or VSS.Unicode.Code_Point (Byte and 16#3F#);

               Seen := Seen + 1;

               if Seen = Needed then
                  if Self.Skip_BOM then
                     Self.Skip_BOM := False;

                     if Code = 16#FEFF# then
                        goto Skip;
                     end if;
                  end if;

                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, Code, Offset);

                  <<Skip>>

                  Needed := 0;
                  Seen   := 0;
               end if;

            else
               Index  := Index - 1;
               Lower  := 16#80#;
               Upper  := 16#BF#;
               Needed := 0;
               Seen   := 0;

               Self.Error := True;

               if Self.Flags (Stop_On_Error) then
                  exit;

               else
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, Replacement_Character, Offset);
               end if;
            end if;

            Index := Index + 1;
         end loop;

         if Self.Flags (Stateless) then
            if Needed /= 0 then
               Self.Error := True;
            end if;

         else
            Self.Code   := Code;
            Self.Needed := Needed;
            Self.Seen   := Seen;
            Self.Upper  := Upper;
            Self.Lower  := Lower;
         end if;
      end if;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : UTF8_Decoder) return VSS.Strings.Virtual_String is
   begin
      if Self.Error then
         return "Iff-formed sequence";

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Error_Message;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error (Self : UTF8_Decoder) return Boolean is
   begin
      return Self.Error;
   end Has_Error;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self  : in out UTF8_Decoder;
      Flags : Converter_Flags) is
   begin
      Self.Flags  := Flags;
      Self.Reset_State;
   end Initialize;

   -----------------
   -- Reset_State --
   -----------------

   overriding procedure Reset_State (Self : in out UTF8_Decoder) is
   begin
      Self.Code     := 0;
      Self.Needed   := 0;
      Self.Seen     := 0;
      Self.Lower    := 16#80#;
      Self.Upper    := 16#BF#;
      Self.Error    := False;
      Self.Skip_BOM := Self.Flags (Process_BOM);
   end Reset_State;

end VSS.Strings.Converters.Decoders.UTF8;
