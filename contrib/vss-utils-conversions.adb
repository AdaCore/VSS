--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;

with VSS.Strings.Conversions;
with VSS.Strings.Converters.Decoders;

package body VSS.Utils.Conversions is

   ------------
   -- Decode --
   ------------

   function Decode
     (Item     : String;
      Encoding : String) return VSS.Strings.Virtual_String
   is
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
      Data    : Ada.Streams.Stream_Element_Array (1 .. Item'Length)
        with Import, Convention => Ada, Address => Item'Address;

   begin
      Decoder.Initialize
        (VSS.Strings.Conversions.To_Virtual_String (Encoding),
         (VSS.Strings.Converters.Stateless => True, others => False));

      if not Decoder.Is_Valid then
         --  Encoding is not supported.

         raise Constraint_Error;
      end if;

      return Result : constant VSS.Strings.Virtual_String :=
        Decoder.Decode (Data)
      do
         if Decoder.Has_Error then
            --  Decoding error.

            raise Constraint_Error;
         end if;
      end return;
   end Decode;

end VSS.Utils.Conversions;
