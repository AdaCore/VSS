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

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Gen_UCD.Data_File_Loaders;
with Gen_UCD.Properties;

package body Gen_UCD.Property_Aliases_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Loader : Gen_UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropertyAliases.txt");

      while not Loader.End_Of_File loop
         declare
            P : constant Properties.Property_Access := new Properties.Property;

         begin
            P.Names.Append
              (To_Unbounded_Wide_Wide_String (Loader.Get_Field (0)));

            --  Second field is a long name of the property and may be the same
            --  as short name of the property, thus ignore it in such cases.

            declare
               Name : constant Unbounded_Wide_Wide_String :=
                 To_Unbounded_Wide_Wide_String (Loader.Get_Field (1));

            begin
               if Name /= P.Names.First_Element then
                  P.Names.Append (Name);
               end if;
            end;

            for J in 2 .. Data_File_Loaders.Field_Index'Last loop
               if Loader.Has_Field (J) then
                  P.Names.Append
                    (To_Unbounded_Wide_Wide_String (Loader.Get_Field (J)));
               end if;
            end loop;

            --  Compute some properties of the property.

            if P.Names.First_Element = "ccc" then
               P.Is_Canonical_Combining_Class := True;
            end if;

            --  Register property and its names.

            Properties.All_Properties.Append (P);

            for Name of P.Names loop
               Properties.Name_To_Property.Insert (Name, P);
            end loop;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end Gen_UCD.Property_Aliases_Loader;
