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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;

with GNATCOLL.JSON;

procedure Test_JSON_Reader_GNATCOLL is
   Data     : Ada.Strings.Unbounded.Unbounded_String;
   Document : GNATCOLL.JSON.JSON_Value;

begin
   declare
      File : Ada.Text_IO.File_Type;
      Aux  : Ada.Strings.Unbounded.Unbounded_String;

   begin
      Ada.Text_IO.Open
        (File, Ada.Text_IO.In_File, Ada.Command_Line.Argument (1));

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Strings.Unbounded.Text_IO.Get_Line (File, Aux);
         --  Ada.Strings.Unbounded.Append (Data, ASCII.LF);
         Ada.Strings.Unbounded.Append (Data, Aux);
      end loop;

      Ada.Text_IO.Close (File);
   end;

   declare
      use type Ada.Calendar.Time;

      Start : Ada.Calendar.Time := Ada.Calendar.Clock;

   begin
      Document := GNATCOLL.JSON.Read (Data);
      Ada.Text_IO.Put_Line (Duration'Image (Ada.Calendar.Clock - Start));
   end;
end Test_JSON_Reader_GNATCOLL;
