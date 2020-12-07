------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with VSS.Stream_Element_Buffers;

procedure Test_Stream_Element_Buffer is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   procedure Test_Assignment_And_Modification;
   --  Test assignment of the buffer's variables and modification of both
   --  buffers.

   procedure Test_Element_Iterator;
   --  Test element iterator.

   --------------------------------------
   -- Test_Assignment_And_Modification --
   --------------------------------------

   procedure Test_Assignment_And_Modification is
      Buffer_1 : VSS.Stream_Element_Buffers.Stream_Element_Buffer;
      Buffer_2 : VSS.Stream_Element_Buffers.Stream_Element_Buffer;

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

   ---------------------------
   -- Test_Element_Iterator --
   ---------------------------

   procedure Test_Element_Iterator is
      Buffer : VSS.Stream_Element_Buffers.Stream_Element_Buffer;
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
   end Test_Element_Iterator;

begin
   Test_Assignment_And_Modification;
   Test_Element_Iterator;
end Test_Stream_Element_Buffer;
