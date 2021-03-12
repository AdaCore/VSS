------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

with VSS.Strings.Conversions;
with VSS.Strings.Converters.Decoders;

package body String_Utilities is

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

end String_Utilities;
