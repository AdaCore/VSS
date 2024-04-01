--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Text_Handlers;

package body VSS.Strings.Converters.Decoders.ISO88599 is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out ISO88599_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (End_Of_Data);

      use type Ada.Streams.Stream_Element_Offset;

      Index   : Ada.Streams.Stream_Element_Offset := Source'First;
      Byte    : Ada.Streams.Stream_Element;
      Offset  : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);
      Handler : VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
      loop
         exit when Index > Source'Last;

         Byte := Source (Index);

         case Byte is
            when 16#D0# =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, 16#011E#, Offset);

            when 16#DD# =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, 16#0130#, Offset);

            when 16#DE# =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, 16#015E#, Offset);

            when 16#F0# =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, 16#011F#, Offset);

            when 16#FD# =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, 16#0131#, Offset);

            when 16#FE# =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, 16#015F#, Offset);

            when others =>
               Handler := VSS.Implementation.Strings.Variable_Handler (Target);
               Handler.Append (Target, VSS.Unicode.Code_Point (Byte), Offset);
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
