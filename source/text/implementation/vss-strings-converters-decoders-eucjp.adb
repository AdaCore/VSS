--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces;

with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;
with VSS.Strings.Converters.Decoders.EUCJP.JIS0212;
with VSS.Strings.Converters.Decoders.Index_JIS0208;

package body VSS.Strings.Converters.Decoders.EUCJP is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out EUCJP_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Text        : out VSS.Implementation.UTF8_Strings.UTF8_String_Data)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      use type VSS.Unicode.Code_Point;

      Index   : Ada.Streams.Stream_Element_Offset := Source'First;
      JIS0212 : Boolean                           := Self.JIS0212;
      Lead    : Ada.Streams.Stream_Element        := Self.Lead;
      Byte    : Ada.Streams.Stream_Element;
      Offset  : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

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
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text, Replacement_Character, Offset);
               end if;
            end if;

            exit;
         end if;

         Byte := Source (Index);

         if Lead = 16#8E# and Byte in 16#A1# .. 16#DF# then
            Lead := 0;

            VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
              (Text,
               16#FF61# - 16#A1# + VSS.Unicode.Code_Point (Byte),
               Offset);

         elsif Lead = 16#8F# and Byte in 16#A1# .. 16#FE# then
            JIS0212 := True;
            Lead    := Byte;

         elsif Lead /= 16#00# then
            declare
               use type Interfaces.Unsigned_32;

               Code : VSS.Unicode.Code_Point := 0;

            begin
               if Lead in 16#A1# .. 16#FE#
                 and Byte in 16#A1# .. 16#FE#
               then
                  declare
                     Pointer : constant Interfaces.Unsigned_32 :=
                       (Interfaces.Unsigned_32 (Lead) - 16#A1#) * 94
                         + Interfaces.Unsigned_32 (Byte) - 16#A1#;

                  begin
                     if JIS0212 then
                        Code := EUCJP.JIS0212.Table (Pointer);

                     else
                        Code := Index_JIS0208.Table (Pointer);
                     end if;
                  end;
               end if;

               Lead    := 0;
               JIS0212 := False;

               if Code /= 0 then
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text, Code, Offset);

               else
                  if Byte in ASCII_Byte_Range then
                     Index := Index - 1;
                  end if;

                  Self.Error := True;

                  if Self.Flags (Stop_On_Error) then
                     exit;

                  else
                     VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                       (Text, Replacement_Character, Offset);
                  end if;
               end if;
            end;

         elsif Byte in ASCII_Byte_Range then
            --  Encoding Standard (Jul 2022) maps 0x5C and 0x7E bytes to
            --  Unicode code point with same value. However, WPT assumes
            --  mapping of 0x5C to U+00A5 and 0x7E to U+203E.

            case Byte is
               when 16#5C# =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text, 16#A5#, Offset);

               when 16#7E# =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text, 16#203E#, Offset);

               when others =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text, VSS.Unicode.Code_Point (Byte), Offset);
            end case;

         elsif Byte in 16#8E# | 16#8F# | 16#A1# .. 16#FE# then
            Lead := Byte;

         else
            Self.Error := True;

            if Self.Flags (Stop_On_Error) then
               exit;

            else
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, Replacement_Character, Offset);
            end if;
         end if;

         Index := Index + 1;
      end loop;

      Self.JIS0212 := JIS0212;
      Self.Lead    := Lead;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : EUCJP_Decoder) return VSS.Strings.Virtual_String is
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
          new EUCJP_Decoder
      do
         declare
            Self : EUCJP_Decoder renames EUCJP_Decoder (Result.all);

         begin
            Self.Flags := Flags;
            Self.Reset_State;
         end;
      end return;
   end Factory;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error (Self : EUCJP_Decoder) return Boolean is
   begin
      return Self.Error;
   end Has_Error;

   -----------------
   -- Reset_State --
   -----------------

   overriding procedure Reset_State (Self : in out EUCJP_Decoder) is
   begin
      Self.JIS0212 := False;
      Self.Lead    := 0;
      Self.Error   := False;
   end Reset_State;

end VSS.Strings.Converters.Decoders.EUCJP;
