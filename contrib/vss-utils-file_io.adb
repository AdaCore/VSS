--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams.Stream_IO;

with VSS.Strings.Conversions;
with VSS.Strings.Converters.Decoders;

package body VSS.Utils.File_IO is

   ----------
   -- Load --
   ----------

   function Load
     (Name : String) return VSS.Stream_Element_Vectors.Stream_Element_Vector
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4_096);
      Last   : Ada.Streams.Stream_Element_Count;
      File   : Ada.Streams.Stream_IO.File_Type;

   begin
      return Result : VSS.Stream_Element_Vectors.Stream_Element_Vector do
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

end VSS.Utils.File_IO;
