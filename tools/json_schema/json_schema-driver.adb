------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
with Ada.Command_Line;

--  with VSS.Application;
with VSS.JSON.Pull_Readers.Simple;
with VSS.Stream_Element_Vectors;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;

with JSON_Schema.Readers;
with JSON_Schema.Writers.Types;

procedure JSON_Schema.Driver is
   Arg    : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String
       (Ada.Command_Line.Argument (1));
   --     VSS.Application.Arguments.Element (1);
   File  : Ada.Streams.Stream_IO.File_Type;
   Raw   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
   Input : aliased VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

   Reader : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   Schema : JSON_Schema.Schema_Access;
   Other  : JSON_Schema.Readers.Schema_Map;
begin
   Ada.Streams.Stream_IO.Open
     (File,
      Ada.Streams.Stream_IO.In_File,
      VSS.Strings.Conversions.To_UTF_8_String (Arg));

   while not Ada.Streams.Stream_IO.End_Of_File (File) loop
      declare
         Data : Ada.Streams.Stream_Element_Array (1 .. 256);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (File, Data, Last);
         for X of Data (1 .. Last) loop
            Raw.Append (X);
         end loop;
      end;
   end loop;

   Input.Set_Data (Raw);
   Reader.Set_Stream (Input'Unchecked_Access);
   Reader.Read_Next;
   pragma Assert (Reader.Is_Start_Document);
   Reader.Read_Next;

   JSON_Schema.Readers.Read (Reader, Schema, Other);
   JSON_Schema.Writers.Types.Write (Other);
end JSON_Schema.Driver;
