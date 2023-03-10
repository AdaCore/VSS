--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces;

with VSS.Implementation.String_Handlers;

with VSS.Strings.Converters.Decoders.GB18030.Indices;
with VSS.Strings.Converters.Decoders.GB18030.Ranges;

package body VSS.Strings.Converters.Decoders.GB18030 is

   function Index_GB18030_Ranges_Code_Point
     (First  : Ada.Streams.Stream_Element;
      Second : Ada.Streams.Stream_Element;
      Third  : Ada.Streams.Stream_Element;
      Byte   : Ada.Streams.Stream_Element)
      return VSS.Unicode.Code_Point;

   function Index_Code_Point
     (First  : Ada.Streams.Stream_Element;
      Byte   : Ada.Streams.Stream_Element)
      return VSS.Unicode.Code_Point
     with Pre => First in 16#81# .. 16#FE#;

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out GB18030_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      use type VSS.Unicode.Code_Point;

      Index  : Ada.Streams.Stream_Element_Offset := Source'First;
      First  : Ada.Streams.Stream_Element        := Self.First;
      Second : Ada.Streams.Stream_Element        := Self.Second;
      Third  : Ada.Streams.Stream_Element        := Self.Third;
      Byte   : Ada.Streams.Stream_Element;
      Offset : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      if Self.Error and Self.Flags (Stop_On_Error) then
         --  Error was encountered in "stop on error" mode, return immidiately.

         return;
      end if;

      loop
         if Index > Source'Last then
            if (First /= 0 or Second /= 0 or Third /= 0)
              and (Self.Flags (Stateless) or End_Of_Data)
            then
               First  := 0;
               Second := 0;
               Third  := 0;

               Self.Error := True;

               if not Self.Flags (Stop_On_Error) then
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, Replacement_Character, Offset);
               end if;
            end if;

            exit;
         end if;

         Byte := Source (Index);

         if Third /= 0 then
            case Byte is
               when 16#30# .. 16#39# =>
                  declare
                     Code : constant VSS.Unicode.Code_Point :=
                       Index_GB18030_Ranges_Code_Point
                         (First, Second, Third, Byte);

                  begin
                     First  := 0;
                     Second := 0;
                     Third  := 0;

                     if Code = 0 then
                        Self.Error := True;

                        if Self.Flags (Stop_On_Error) then
                           exit;

                        else
                           VSS.Implementation.Strings.Handler (Target).Append
                             (Target, Replacement_Character, Offset);
                        end if;

                     else
                        VSS.Implementation.Strings.Handler (Target).Append
                          (Target, Code, Offset);
                     end if;
                  end;

               when others =>
                  declare
                     Buffer : constant Ada.Streams.Stream_Element_Array :=
                       (Second, Third, Byte);

                  begin
                     First  := 0;
                     Second := 0;
                     Third  := 0;

                     Self.Error := True;

                     if Self.Flags (Stop_On_Error) then
                        exit;

                     else
                        VSS.Implementation.Strings.Handler (Target).Append
                          (Target, Replacement_Character, Offset);
                     end if;

                     Self.First  := First;
                     Self.Second := Second;
                     Self.Third  := Third;

                     Self.Decode (Buffer, False, Target);

                     First  := Self.First;
                     Second := Self.Second;
                     Third  := Self.Third;
                  end;
            end case;

         elsif Second /= 0 then
            case Byte is
               when 16#81# .. 16#FE# =>
                  Third := Byte;

               when others =>
                  declare
                     Buffer : constant Ada.Streams.Stream_Element_Array :=
                       (Second, Byte);

                  begin
                     First  := 0;
                     Second := 0;

                     Self.Error := True;

                     if Self.Flags (Stop_On_Error) then
                        exit;

                     else
                        VSS.Implementation.Strings.Handler (Target).Append
                          (Target, Replacement_Character, Offset);
                     end if;

                     Self.First  := First;
                     Self.Second := Second;
                     Self.Third  := Third;

                     Self.Decode (Buffer, False, Target);

                     First  := Self.First;
                     Second := Self.Second;
                     Third  := Self.Third;
                  end;
            end case;

         elsif First /= 0 then
            case Byte is
               when 16#30# .. 16#39# =>
                  Second := Byte;

               when others =>
                  declare
                     Code : constant VSS.Unicode.Code_Point :=
                       Index_Code_Point (First, Byte);

                  begin
                     First := 0;

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
            end case;

         else
            case Byte is
               when ASCII_Byte_Range =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, VSS.Unicode.Code_Point (Byte), Offset);

               when 16#80# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#20AC#, Offset);

               when 16#81# .. 16#FE# =>
                  First := Byte;

               when others =>
                  Self.Error := True;

                  if Self.Flags (Stop_On_Error) then
                     exit;

                  else
                     VSS.Implementation.Strings.Handler (Target).Append
                       (Target, Replacement_Character, Offset);
                  end if;
            end case;
         end if;

         Index := Index + 1;
      end loop;

      Self.First  := First;
      Self.Second := Second;
      Self.Third  := Third;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : GB18030_Decoder) return VSS.Strings.Virtual_String is
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
          new GB18030_Decoder
      do
         declare
            Self : GB18030_Decoder renames GB18030_Decoder (Result.all);

         begin
            Self.Flags := Flags;
            Self.Reset_State;
         end;
      end return;
   end Factory;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error (Self : GB18030_Decoder) return Boolean is
   begin
      return Self.Error;
   end Has_Error;

   ----------------------
   -- Index_Code_Point --
   ----------------------

   function Index_Code_Point
     (First  : Ada.Streams.Stream_Element;
      Byte   : Ada.Streams.Stream_Element) return VSS.Unicode.Code_Point
   is
      use type Ada.Streams.Stream_Element;
      use type Interfaces.Unsigned_32;

      Offset  : constant Interfaces.Unsigned_32 :=
        (if Byte < 16#7F# then 16#40# else 16#41#);
      Pointer : Interfaces.Unsigned_32 := 0;

   begin
      case Byte is
         when 16#40# .. 16#7E# | 16#80# .. 16#FE# =>
            Pointer :=
              (Interfaces.Unsigned_32 (First) - 16#81#) * 190
                 + (Interfaces.Unsigned_32 (Byte) - Offset);

            return VSS.Unicode.Code_Point (Indices.Table (Pointer));

         when others =>
            return 0;
      end case;
   end Index_Code_Point;

   -------------------------------------
   -- Index_GB18030_Ranges_Code_Point --
   -------------------------------------

   function Index_GB18030_Ranges_Code_Point
     (First  : Ada.Streams.Stream_Element;
      Second : Ada.Streams.Stream_Element;
      Third  : Ada.Streams.Stream_Element;
      Byte   : Ada.Streams.Stream_Element)
      return VSS.Unicode.Code_Point
   is
      use type Interfaces.Unsigned_32;

      Pointer           : constant Interfaces.Unsigned_32 :=
        (Interfaces.Unsigned_32 (First) - 16#81#) * 10 * 126 * 10
          + (Interfaces.Unsigned_32 (Second) - 16#30#) * 10 * 126
          + (Interfaces.Unsigned_32 (Third) - 16#81#) * 10
          + Interfaces.Unsigned_32 (Byte) - 16#30#;
      Pointer_Offset    : Interfaces.Unsigned_32 :=
        Ranges.Table (Ranges.Table'First).Pointer_Offset;
      Code_Point_Offset : Interfaces.Unsigned_32 :=
        Ranges.Table (Ranges.Table'First).Code_Point_Offset;

   begin
      if Pointer in 39_420 .. 188_999 or Pointer > 1_237_575 then
         return 0;
      end if;

      if Pointer = 7457 then
         return 16#E7C7#;
      end if;

      for J in Ranges.Table'Range loop
         exit when Ranges.Table (J).Pointer_Offset > Pointer;

         Pointer_Offset    := Ranges.Table (J).Pointer_Offset;
         Code_Point_Offset := Ranges.Table (J).Code_Point_Offset;
      end loop;

      return
        VSS.Unicode.Code_Point (Code_Point_Offset + Pointer - Pointer_Offset);
   end Index_GB18030_Ranges_Code_Point;

   -----------------
   -- Reset_State --
   -----------------

   overriding procedure Reset_State (Self : in out GB18030_Decoder) is
   begin
      Self.First  := 0;
      Self.Second := 0;
      Self.Third  := 0;
      Self.Error  := False;
   end Reset_State;

end VSS.Strings.Converters.Decoders.GB18030;
