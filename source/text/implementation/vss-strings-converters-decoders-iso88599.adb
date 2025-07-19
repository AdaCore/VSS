--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;

package body VSS.Strings.Converters.Decoders.ISO88599 is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out ISO88599_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Text        : out VSS.Implementation.UTF8_Strings.UTF8_String_Data)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (End_Of_Data);

      use type Ada.Streams.Stream_Element_Offset;

      Index  : Ada.Streams.Stream_Element_Offset := Source'First;
      Byte   : Ada.Streams.Stream_Element;
      Offset : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      loop
         exit when Index > Source'Last;

         Byte := Source (Index);

         case Byte is
            when 16#D0# =>
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, 16#011E#, Offset);

            when 16#DD# =>
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, 16#0130#, Offset);

            when 16#DE# =>
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, 16#015E#, Offset);

            when 16#F0# =>
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, 16#011F#, Offset);

            when 16#FD# =>
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, 16#0131#, Offset);

            when 16#FE# =>
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, 16#015F#, Offset);

            when others =>
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Text, VSS.Unicode.Code_Point (Byte), Offset);
         end case;

         Index := Index + 1;
      end loop;
   end Decode;

   -------------
   -- Factory --
   -------------

   function Factory
     (Flags : Converter_Flags)
      return VSS.Strings.Converters.Decoders.Decoder_Access
   is
      pragma Unreferenced (Flags);

   begin
      return new ISO88599_Decoder;
   end Factory;

end VSS.Strings.Converters.Decoders.ISO88599;
