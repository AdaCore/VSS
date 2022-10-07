--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Configuration;

package body VSS.Strings.Converters.Decoders.ISO885915 is

   ------------
   -- Decode --
   ------------

   overriding procedure Decode
     (Self   : in out ISO885915_Decoder;
      Source : Ada.Streams.Stream_Element_Array;
      Target : out VSS.Implementation.Strings.String_Data)
   is
      pragma Unreferenced (Self);

      use type Ada.Streams.Stream_Element_Offset;

      Index  : Ada.Streams.Stream_Element_Offset := Source'First;
      Byte   : Ada.Streams.Stream_Element;
      Offset : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      if Source'Last < Source'First then
         --  Source data is empty: return "null" string.

         Target := VSS.Implementation.Strings.Null_String_Data;

      else
         VSS.Implementation.String_Configuration.In_Place_Handler.Initialize
           (Target);

         loop
            exit when Index > Source'Last;

            Byte := Source (Index);

            case Byte is
               when 16#A4# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#20AC#, Offset);

               when 16#A6# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#0160#, Offset);

               when 16#A8# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#0161#, Offset);

               when 16#B4# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#017D#, Offset);

               when 16#B8# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#017E#, Offset);

               when 16#BC# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#0152#, Offset);

               when 16#BD# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#0153#, Offset);

               when 16#BE# =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, 16#0178#, Offset);

               when others =>
                  VSS.Implementation.Strings.Handler (Target).Append
                    (Target, VSS.Unicode.Code_Point (Byte), Offset);
            end case;

            Index := Index + 1;
         end loop;
      end if;
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
      return new ISO885915_Decoder;
   end Factory;

end VSS.Strings.Converters.Decoders.ISO885915;
