--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;

with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

procedure Test_Text_Streams is

   procedure First_Test;
   --  First added test, to check some code point inside particular encoded
   --  sequence length. Output stream only.

   ----------------
   -- First_Test --
   ----------------

   procedure First_Test is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      Stream  : VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;
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
   end First_Test;

   procedure Markus_Kuhn_Test is separate;
   --  Test based on Markus Kuhn's work, see
   --  https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt

begin
   First_Test;
   Markus_Kuhn_Test;
end Test_Text_Streams;
