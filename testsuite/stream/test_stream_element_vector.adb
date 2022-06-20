--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;

with VSS.Stream_Element_Vectors.Conversions;

with Test_Support;

procedure Test_Stream_Element_Vector is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   procedure Test_Assignment_And_Modification;
   --  Test assignment of the buffer's variables and modification of both
   --  buffers.

   procedure Test_Element_Iterator;
   --  Test element iterator.

   procedure Test_Conversions;
   --  Test conversions from/to standard String/Unbounded_String;

   --------------------------------------
   -- Test_Assignment_And_Modification --
   --------------------------------------

   procedure Test_Assignment_And_Modification is
      Buffer_1 : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Buffer_2 : VSS.Stream_Element_Vectors.Stream_Element_Vector;

   begin
      --  Fill buffer

      for C in reverse Character'('A') .. Character'('Z') loop
         Buffer_1.Append (Character'Pos (C));
      end loop;

      Buffer_2 := Buffer_1;

      Buffer_1.Append (Character'Pos ('1'));
      Buffer_2.Append (Character'Pos ('2'));

      if Buffer_1.Element (Buffer_1.Length) /= Character'Pos ('1') then
         raise Program_Error;
      end if;

      if Buffer_2.Element (Buffer_2.Length) /= Character'Pos ('2') then
         raise Program_Error;
      end if;
   end Test_Assignment_And_Modification;

   ----------------------
   -- Test_Conversions --
   ----------------------

   procedure Test_Conversions is separate;

   ---------------------------
   -- Test_Element_Iterator --
   ---------------------------

   procedure Test_Element_Iterator is
      Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Count  : Ada.Streams.Stream_Element_Offset := 0;

   begin
      --  Fill buffer

      for C in reverse Character'('A') .. Character'('Z') loop
         Buffer.Append (Character'Pos (C));
      end loop;

      if Buffer.Length /= Character'Pos ('Z') - Character'Pos ('A') + 1 then
         raise Program_Error;
      end if;

      --  Check content of the buffer

      for J in 1 .. Buffer.Length loop
         Count := Count + 1;

         if Buffer (J)
           /= Ada.Streams.Stream_Element (Character'Pos ('Z') - J + 1)
         then
            raise Program_Error;
         end if;
      end loop;

      --  Chech that previous loop has been executed expected number of times.

      if Count /= Buffer.Length then
         raise Program_Error;
      end if;

      --  Check content of the vector using Ada 2012 iterator and syntax sugar
      --  (normal order)

      Count := 0;

      for E of Buffer loop
         Count := Count + 1;

         Test_Support.Assert
           (E = Ada.Streams.Stream_Element (Character'Pos ('Z') - Count + 1));
      end loop;

      --  Check that previous loop has been executed expected number of times.

      Test_Support.Assert (Count = Buffer.Length);

      --  Check content of the vector using Ada 2012 iterator and syntax sugar
      --  (reverse order)

      Count := Buffer.Length;

      for E of reverse Buffer loop
         Test_Support.Assert
           (E = Ada.Streams.Stream_Element (Character'Pos ('Z') - Count + 1));

         Count := Count - 1;
      end loop;

      --  Check that previous loop has been executed expected number of times.

      Test_Support.Assert (Count = 0);
   end Test_Element_Iterator;

begin
   Test_Assignment_And_Modification;
   Test_Element_Iterator;
   Test_Conversions;
end Test_Stream_Element_Vector;
