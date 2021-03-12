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

with Ada.Streams.Stream_IO;

with VSS.Strings.Conversions;
with VSS.Strings.Converters.Decoders;

package body File_Utilities is

   ----------
   -- Load --
   ----------

   function Load
     (Name : String) return VSS.Stream_Element_Buffers.Stream_Element_Buffer
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4_096);
      Last   : Ada.Streams.Stream_Element_Count;
      File   : Ada.Streams.Stream_IO.File_Type;

   begin
      return Result : VSS.Stream_Element_Buffers.Stream_Element_Buffer do
         Ada.Streams.Stream_IO.Open
           (File, Ada.Streams.Stream_IO.In_File, Name);

         while not Ada.Streams.Stream_IO.End_Of_File (File) loop
            Ada.Streams.Stream_IO.Read (File, Buffer, Last);

            for J in Buffer'First .. Last loop
               Result.Append (Buffer (J));
            end loop;
         end loop;

         Ada.Streams.Stream_IO.Close (File);
      end return;

   exception
      when others =>
         Ada.Streams.Stream_IO.Close (File);

         raise;
   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (Name     : String;
      Encoding : String) return VSS.Strings.Virtual_String
   is
      Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;

   begin
      Decoder.Initialize
        (VSS.Strings.Conversions.To_Virtual_String (Encoding),
         (VSS.Strings.Converters.Stateless => True, others => False));

      if not Decoder.Is_Valid then
         --  Encoding is not supported.

         raise Constraint_Error;
      end if;

      return Result : constant VSS.Strings.Virtual_String :=
        Decoder.Decode (Load (Name))
      do
         if Decoder.Has_Error then
            --  Decoding error.

            raise Constraint_Error;
         end if;
      end return;
   end Load;

end File_Utilities;
