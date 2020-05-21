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

with Ada.Text_IO;

with Magic.Stream_Element_Buffers;
with Magic.Stream_Element_Buffers.Conversions;
with Magic.Strings.Conversions;
with Magic.Text_Streams.Memory;
with Magic.JSON.Streams.Writers;

procedure Test_JSON_Writer is
   Stream  : aliased Magic.Text_Streams.Memory.Memory_UTF8_Output_Stream;
   Writer  : aliased Magic.JSON.Streams.Writers.JSON_Simple_Writer;
   Success : Boolean := True;

begin
   Writer.Set_Stream (Stream'Unchecked_Access);
   Writer.Start_Document (Success);

   Writer.Start_Object (Success);

   Writer.Key_Name
     (Magic.Strings.Conversions.To_Magic_String ("name"), Success);
   Writer.String_Value
     (Magic.Strings.Conversions.To_Magic_String ("Some name"), Success);
   Writer.Key_Name
     (Magic.Strings.Conversions.To_Magic_String ("names"), Success);
   Writer.Start_Array (Success);
   Writer.String_Value
     (Magic.Strings.Conversions.To_Magic_String ("Some"), Success);
   Writer.String_Value
     (Magic.Strings.Conversions.To_Magic_String ("name"), Success);
   Writer.End_Array (Success);
   Writer.Key_Name
     (Magic.Strings.Conversions.To_Magic_String ("is"), Success);
   Writer.Boolean_Value (False, Success);
   Writer.Key_Name
     (Magic.Strings.Conversions.To_Magic_String ("no"), Success);
   Writer.Boolean_Value (True, Success);
   Writer.Key_Name
     (Magic.Strings.Conversions.To_Magic_String ("empty"), Success);
   Writer.Null_Value (Success);
   Writer.Key_Name
     (Magic.Strings.Conversions.To_Magic_String ("integer"), Success);
   Writer.Integer_Value (15, Success);
   Writer.Key_Name
     (Magic.Strings.Conversions.To_Magic_String ("float"), Success);
   Writer.Float_Value (20.5, Success);

   writer.End_Object (Success);

   Writer.End_Document (Success);

   Ada.Text_IO.Put
     (Magic.Stream_Element_Buffers.Conversions.Unchecked_To_String
        (Stream.Buffer));
end Test_JSON_Writer;
