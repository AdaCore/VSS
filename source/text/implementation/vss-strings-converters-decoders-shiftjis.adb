--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces;

with VSS.Implementation.String_Handlers;

with VSS.Strings.Converters.Decoders.Index_JIS0208;

package body VSS.Strings.Converters.Decoders.ShiftJIS is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out ShiftJIS_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      use type VSS.Unicode.Code_Point;

      Index  : Ada.Streams.Stream_Element_Offset := Source'First;
      Lead   : Ada.Streams.Stream_Element        := Self.Lead;
      Byte   : Ada.Streams.Stream_Element;
      Offset : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      if Self.Error and Self.Flags (Stop_On_Error) then
         --  Error was encountered in "stop on error" mode, return immidiately.

         return;
      end if;

      loop
         if Index > Source'Last then
            if Lead /= 0 and (Self.Flags (Stateless) or End_Of_Data) then
               Lead := 0;

               Self.Error := True;

               if not Self.Flags (Stop_On_Error) then
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, Replacement_Character, Offset);
               end if;
            end if;

            exit;
         end if;

         Byte := Source (Index);

         if Lead /= 0 then
            declare
               --  use type Ada.Streams.Stream_Element;
               use type Interfaces.Unsigned_32;

               Pointer     : Interfaces.Unsigned_32     := 0;
               Byte_Offset : constant Ada.Streams.Stream_Element :=
                 (if Byte < 16#7F# then 16#40# else 16#41#);
               Lead_Offset : constant Ada.Streams.Stream_Element :=
                 (if Lead < 16#A0# then 16#81# else 16#C1#);
               Code        : VSS.Unicode.Code_Point     := 0;

            begin
               if Byte in 16#40# .. 16#7E# | 16#80# .. 16#FC# then
                  Pointer :=
                    Interfaces.Unsigned_32 (Lead - Lead_Offset) * 188
                      + Interfaces.Unsigned_32 (Byte - Byte_Offset);

                  if Pointer in 8_836 .. 10_715 then
                     Code :=
                       VSS.Unicode.Code_Point (16#E000# - 8_836 + Pointer);

                  else
                     Code := Index_JIS0208.Table (Pointer);
                  end if;
               end if;

               Lead := 0;

               if Code /= 0 then
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, Code, Offset);

               else
                  if Byte in ASCII_Byte_Range then
                     Index := Index - 1;
                  end if;

                  Self.Error := True;

                  if Self.Flags (Stop_On_Error) then
                     exit;

                  else
                     VSS.Implementation.Strings.Handler (Target).Append
                       (Target, Replacement_Character, Offset);
                  end if;
               end if;
            end;

         elsif Byte in ASCII_Byte_Range | 16#80# then
            --  Encoding Standard (Jul 2022) maps 0x5C and 0x7E bytes to
            --  Unicode code point with same value. However, WPT assumes
            --  mapping of 0x5C to U+00A5 and 0x7E to U+203E.

            case Byte is
               when 16#5C# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#A5#, Offset);

               when 16#7E# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#203E#, Offset);

               when others =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, VSS.Unicode.Code_Point (Byte), Offset);
            end case;

         elsif Byte in 16#A1# .. 16#DF# then
            VSS.Implementation.Strings.Handler (Target).Append
              (Target,
               16#FF61# + VSS.Unicode.Code_Point (Byte - 16#A1#),
               Offset);

         elsif Byte in 16#81# .. 16#9F# | 16#E0# .. 16#FC# then
            Lead := Byte;

         else
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

      Self.Lead := Lead;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : ShiftJIS_Decoder) return VSS.Strings.Virtual_String is
   begin
      if Self.Error then
         return "Iff-formed sequence";

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Error_Message;

   -------------
   -- Factory --
   -------------

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access is
   begin
      return Result : constant
        VSS.Strings.Converters.Decoders.Decoder_Access :=
          new ShiftJIS_Decoder
      do
         declare
            Self : ShiftJIS_Decoder renames ShiftJIS_Decoder (Result.all);

         begin
            Self.Flags := Flags;
            Self.Reset_State;
         end;
      end return;
   end Factory;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error (Self : ShiftJIS_Decoder) return Boolean is
   begin
      return Self.Error;
   end Has_Error;

   -----------------
   -- Reset_State --
   -----------------

   overriding procedure Reset_State (Self : in out ShiftJIS_Decoder) is
   begin
      Self.Lead  := 0;
      Self.Error := False;
   end Reset_State;

end VSS.Strings.Converters.Decoders.ShiftJIS;
