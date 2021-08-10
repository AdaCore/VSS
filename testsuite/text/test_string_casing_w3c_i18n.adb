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

with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with VSS.Strings;

with Test_Support;

procedure Test_String_Casing_W3C_I18N is
   use type VSS.Strings.Virtual_String;

   File      : Ada.Wide_Wide_Text_IO.File_Type;
   Line      : Wide_Wide_String (1 .. 1024);
   Last      : Natural;
   Lowercase : Boolean;
   Source    : VSS.Strings.Virtual_String;
   Expected  : VSS.Strings.Virtual_String;

begin
   Ada.Wide_Wide_Text_IO.Open
     (File,
      Ada.Wide_Wide_Text_IO.In_File,
      Ada.Command_Line.Argument (1),
      "wcem=8");

   --  Skip name of the test

   Ada.Wide_Wide_Text_IO.Skip_Line (File);

   --  Read case conversion

   Ada.Wide_Wide_Text_IO.Get_Line (File, Line, Last);

   if Line (Line'First .. Last) = "lowercase" then
      Lowercase := True;

   elsif Line (Line'First .. Last) = "uppercase" then
      Lowercase := False;

   else
      raise Program_Error;
   end if;

   --  Read source string

   Ada.Wide_Wide_Text_IO.Get_Line (File, Line, Last);
   Source := VSS.Strings.To_Virtual_String (Line (Line'First .. Last));
   Test_Support.Assert (not Source.Is_Empty);

   --  Read expected string

   Ada.Wide_Wide_Text_IO.Get_Line (File, Line, Last);
   Expected := VSS.Strings.To_Virtual_String (Line (Line'First .. Last));
   Test_Support.Assert (not Expected.Is_Empty);

   Ada.Wide_Wide_Text_IO.Close (File);

   if Lowercase then
      Test_Support.Assert (Source.To_Lowercase = Expected);

   else
      Test_Support.Assert (Source.To_Uppercase = Expected);
   end if;
end Test_String_Casing_W3C_I18N;
