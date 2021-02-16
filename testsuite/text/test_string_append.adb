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

with VSS.Strings;

procedure Test_String_Append is
   use type VSS.Strings.Virtual_String;

   Long_Literal_1 : constant Wide_Wide_String :=
     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
   Long_Literal_2 : constant Wide_Wide_String :=
     "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";

   Short_1 : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String ("A");
   Short_2 : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String ("B");

   Long_1  : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String
     (Long_Literal_1);

   Long_2  : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String
     (Long_Literal_2);
   Copy_1  : constant VSS.Strings.Virtual_String := Short_1;
   Copy_2  : constant VSS.Strings.Virtual_String := Long_2;
begin
   Short_1.Append (Short_2);
   if Short_1 /= VSS.Strings.To_Virtual_String ("AB") then
      raise Program_Error;
   end if;

   Short_2.Append (Long_1);
   if Short_2 /= VSS.Strings.To_Virtual_String ("B" & Long_Literal_1) then
      raise Program_Error;
   end if;

   Long_1.Append (Short_1);
   if Long_1 /= VSS.Strings.To_Virtual_String (Long_Literal_1 & "AB") then
      raise Program_Error;
   end if;

   Long_2.Append (Long_1);
   if Long_2 /= VSS.Strings.To_Virtual_String
     (Long_Literal_2 & Long_Literal_1 & "AB")
   then
      raise Program_Error;
   end if;

   if Copy_1 /= VSS.Strings.To_Virtual_String ("A") then
      raise Program_Error;
   elsif Copy_2 /= VSS.Strings.To_Virtual_String (Long_Literal_2) then
      raise Program_Error;
   end if;

end Test_String_Append;
