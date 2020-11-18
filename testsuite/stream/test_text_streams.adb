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

with VSS.Text_Streams.Memory;

procedure Test_Text_Streams is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   Stream  : VSS.Text_Streams.Memory.Memory_UTF8_Output_Stream;
   Success : Boolean := True;

   Expected : constant Ada.Streams.Stream_Element_Array :=
     (1  => 16#41#,
      2  => 16#D0#,
      3  => 16#91#,
      4  => 16#E0#,
      5  => 16#A4#,
      6  => 16#95#,
      7  => 16#F0#,
      8  => 16#90#,
      9  => 16#8C#,
      10 => 16#88#);

begin
   Stream.Put ('A', Success);
   Stream.Put ('Б', Success);
   Stream.Put ('क', Success);
   Stream.Put ('𐌈', Success);

   if not Success then
      raise Program_Error;
   end if;

   if Stream.Buffer.Length /= Expected'Length then
      raise Program_Error;
   end if;

   for J in Expected'Range loop
      if Stream.Buffer.Element (J) /= Expected (J) then
         raise Program_Error;
      end if;
   end loop;
end Test_Text_Streams;
