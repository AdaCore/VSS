--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Handlers;

package body VSS.Strings.Converters.Decoders.ISO88597 is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out ISO88597_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data)
   is
      pragma Unreferenced (End_Of_Data);

      use type Ada.Streams.Stream_Element_Offset;
      use type VSS.Unicode.Code_Point;

      Index  : Ada.Streams.Stream_Element_Offset := Source'First;
      Byte   : Ada.Streams.Stream_Element;
      Offset : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      if Self.Error and Self.Flags (Stop_On_Error) then
         --  Error was encountered in "stop on error" mode, return immidiately.

         return;
      end if;

      loop
         exit when Index > Source'Last;

         Byte := Source (Index);

         case Byte is
            when 16#00# .. 16#A0# | 16#A3# | 16#A6# .. 16#A9#
               | 16#AB# .. 16#AD# | 16#B0# .. 16#B3# | 16#B7# | 16#BB#
               | 16#BD#
            =>
               VSS.Implementation.Strings.Handler (Target).Append
                 (Target, VSS.Unicode.Code_Point (Byte), Offset);

            when 16#A1# =>
               VSS.Implementation.Strings.Handler (Target).Append
                 (Target, 16#2018#, Offset);

            when 16#A2# =>
               VSS.Implementation.Strings.Handler (Target).Append
                 (Target, 16#2019#, Offset);

            when 16#A4# =>
               VSS.Implementation.Strings.Handler (Target).Append
                 (Target, 16#20AC#, Offset);

            when 16#A5# =>
               VSS.Implementation.Strings.Handler (Target).Append
                 (Target, 16#20AF#, Offset);

            when 16#AF# =>
               VSS.Implementation.Strings.Handler (Target).Append
                 (Target, 16#2015#, Offset);

            when 16#AE# | 16#D2# | 16#FF# =>
               Self.Error := True;

               exit when Self.Flags (Stop_On_Error);

               VSS.Implementation.Strings.Handler (Target).Append
                 (Target, Replacement_Character, Offset);

            when others =>
               VSS.Implementation.Strings.Handler (Target).Append
                 (Target,
                  VSS.Unicode.Code_Point (Byte) - 16#A0# + 16#0370#,
                  Offset);
         end case;

         Index := Index + 1;
      end loop;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : ISO88597_Decoder) return VSS.Strings.Virtual_String is
   begin
      if Self.Error then
         return "Unmapped character";

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
          new ISO88597_Decoder
      do
         declare
            Self : ISO88597_Decoder renames ISO88597_Decoder (Result.all);

         begin
            Self.Flags := Flags;
            Self.Reset_State;
         end;
      end return;
   end Factory;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error (Self : ISO88597_Decoder) return Boolean is
   begin
      return Self.Error;
   end Has_Error;

   -----------------
   -- Reset_State --
   -----------------

   overriding procedure Reset_State (Self : in out ISO88597_Decoder) is
   begin
      Self.Error := False;
   end Reset_State;

end VSS.Strings.Converters.Decoders.ISO88597;
