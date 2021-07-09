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

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Gen_UCD.Data_File_Loaders;

package body Gen_UCD.Property_Aliases_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Loader : Gen_UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropertyAliases.txt");

      while not Loader.End_Of_File loop
         Put
           ("Property '" & Loader.Get_Field (0)
            & "': '" & Loader.Get_Field (1) &  ''');

         for J in 2 .. Data_File_Loaders.Field_Index'Last loop
            if Loader.Has_Field (J) then
               Put (", '" & Loader.Get_Field (J) & ''');
            end if;
         end loop;

         New_Line;

         declare
            P : constant Property_Access := new Property;
            N : Unbounded_Wide_Wide_String;

         begin
            N := To_Unbounded_Wide_Wide_String (Loader.Get_Field (0));
            P.Names.Append (N);
            Properties.Insert (N, P);

            --  Second field is a long name of the property and may be the same
            --  as short name of the property, thus ignore it in such cases.

            N := To_Unbounded_Wide_Wide_String (Loader.Get_Field (1));

            if N /= P.Names.First_Element then
               P.Names.Append (N);
               Properties.Insert (N, P);
            end if;

            for J in 2 .. Data_File_Loaders.Field_Index'Last loop
               if Loader.Has_Field (J) then
                  N := To_Unbounded_Wide_Wide_String (Loader.Get_Field (J));
                  P.Names.Append (N);
                  Properties.Insert (N, P);
               end if;
            end loop;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end Gen_UCD.Property_Aliases_Loader;
