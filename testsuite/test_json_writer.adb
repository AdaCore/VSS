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

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;

with VSS.Stream_Element_Buffers.Conversions;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory;
with VSS.JSON.Streams.Writers;

procedure Test_JSON_Writer is

   use type Interfaces.IEEE_Float_64;
   use type Interfaces.Integer_64;

   Stream  : aliased VSS.Text_Streams.Memory.Memory_UTF8_Output_Stream;
   Writer  : aliased VSS.JSON.Streams.Writers.JSON_Simple_Writer;
   Success : Boolean := True;

begin
   Writer.Set_Stream (Stream'Unchecked_Access);
   Writer.Start_Document (Success);

   Writer.Start_Object (Success);

   --  Some usual constructs.

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("name"), Success);
   Writer.String_Value
     (VSS.Strings.Conversions.To_Magic_String ("Some name"), Success);
   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("names"), Success);
   Writer.Start_Array (Success);
   Writer.String_Value
     (VSS.Strings.Conversions.To_Magic_String ("Some"), Success);
   Writer.String_Value
     (VSS.Strings.Conversions.To_Magic_String ("name"), Success);
   Writer.End_Array (Success);
   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("is"), Success);
   Writer.Boolean_Value (False, Success);
   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("no"), Success);
   Writer.Boolean_Value (True, Success);
   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("empty"), Success);
   Writer.Null_Value (Success);
   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("integer"), Success);
   Writer.Integer_Value (15, Success);
   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("float"), Success);
   Writer.Float_Value (20.5, Success);

   --  Arrays of different types

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("booleans"), Success);
   Writer.Start_Array (Success);
   Writer.Boolean_Value (False, Success);
   Writer.Boolean_Value (True, Success);
   Writer.End_Array (Success);

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("nulls"), Success);
   Writer.Start_Array (Success);
   Writer.Null_Value (Success);
   Writer.Null_Value (Success);
   Writer.End_Array (Success);

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("floats"), Success);
   Writer.Start_Array (Success);
   Writer.Float_Value (-1.0, Success);
   Writer.Float_Value (1.0, Success);
   Writer.End_Array (Success);

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("integers"), Success);
   Writer.Start_Array (Success);
   Writer.Integer_Value (-1, Success);
   Writer.Integer_Value (1, Success);
   Writer.End_Array (Success);

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("arrays"), Success);
   Writer.Start_Array (Success);
   Writer.Start_Array (Success);
   Writer.End_Array (Success);
   Writer.Start_Array (Success);
   Writer.End_Array (Success);
   Writer.End_Array (Success);

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("objects"), Success);
   Writer.Start_Array (Success);
   Writer.Start_Object (Success);
   Writer.End_Object (Success);
   Writer.Start_Object (Success);
   Writer.End_Object (Success);
   Writer.End_Array (Success);

   --  All control characters inside string value

   Writer.Key_Name
     (VSS.Strings.Conversions.To_Magic_String ("controls"), Success);
   Writer.String_Value
     (VSS.Strings.Conversions.To_Magic_String
        ((Character'Val (16#00#),
         Character'Val (16#01#),
         Character'Val (16#02#),
         Character'Val (16#03#),
         Character'Val (16#04#),
         Character'Val (16#05#),
         Character'Val (16#06#),
         Character'Val (16#07#),
         Character'Val (16#08#),
         Character'Val (16#09#),
         Character'Val (16#0A#),
         Character'Val (16#0B#),
         Character'Val (16#0C#),
         Character'Val (16#0D#),
         Character'Val (16#0E#),
         Character'Val (16#0F#),
         Character'Val (16#10#),
         Character'Val (16#11#),
         Character'Val (16#12#),
         Character'Val (16#13#),
         Character'Val (16#14#),
         Character'Val (16#15#),
         Character'Val (16#16#),
         Character'Val (16#17#),
         Character'Val (16#18#),
         Character'Val (16#19#),
         Character'Val (16#1A#),
         Character'Val (16#1B#),
         Character'Val (16#1C#),
         Character'Val (16#1D#),
         Character'Val (16#1E#),
         Character'Val (16#1F#))),
      Success);

   writer.End_Object (Success);

   Writer.End_Document (Success);

   declare
      File : Ada.Streams.Stream_IO.File_Type;

   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));

      declare
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Offset;

         Expected : Ada.Streams.Stream_Element_Array
           (1 .. Ada.Streams.Stream_Element_Count
                   (Ada.Streams.Stream_IO.Size (File)));
         Last     : Ada.Streams.Stream_Element_Count;

      begin
         Ada.Streams.Stream_IO.Read (File, Expected, Last);

         if Last /= Expected'Last or Last = 0 then
            raise Program_Error;
         end if;

         for J in Expected'Range loop
            if Expected (J) /= Stream.Buffer.Element (J) then
               Ada.Text_IO.Put
                 (VSS.Stream_Element_Buffers.Conversions.Unchecked_To_String
                    (Stream.Buffer));

               raise Program_Error;
            end if;
         end loop;
      end;
   end;
end Test_JSON_Writer;
