--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.String_Handlers;

package body VSS.Strings.Converters.Decoders.ISO88591 is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self        : in out ISO88591_Decoder;
      Source      : Ada.Streams.Stream_Element_Array;
      End_Of_Data : Boolean;
      Target      : out VSS.Implementation.Strings.String_Data)
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

         VSS.Implementation.Strings.Handler (Target).Append
           (Target, VSS.Unicode.Code_Point (Byte), Offset);

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
      return new ISO88591_Decoder;
   end Factory;

end VSS.Strings.Converters.Decoders.ISO88591;
