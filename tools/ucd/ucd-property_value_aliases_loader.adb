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

with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Property_Value_Aliases_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropertyValueAliases.txt");

      while not Loader.End_Of_File loop
         --  XXX @missing annotation is not processed!

         declare
            P : constant Properties.Property_Access :=
              Properties.Name_To_Property
                (To_Unbounded_Wide_Wide_String (Loader.Get_Field (0)));
            V : constant Properties.Property_Value_Access :=
              new Properties.Property_Value;
            F : Data_File_Loaders.Field_Index := 1;

         begin
            if P.Is_Canonical_Combining_Class then
               --  Second field for 'ccc' property is numeric value.

               V.Canonical_Combining_Class_Value :=
                 Properties.Canonical_Combinig_Class'Wide_Wide_Value
                   (Loader.Get_Field (1));
               F := 2;
            end if;

            for J in F .. Data_File_Loaders.Field_Index'Last loop
               if Loader.Has_Field (J) then
                  declare
                     Name : constant Unbounded_Wide_Wide_String :=
                       To_Unbounded_Wide_Wide_String (Loader.Get_Field (J));

                  begin
                     --  Short name and long name may be the same, ignore
                     --  duplicates.

                     if V.Names.Is_Empty
                          or else Name /= V.Names.First_Element
                     then
                        V.Names.Append (Name);
                     end if;
                  end;
               end if;
            end loop;

            --  Register value and its names

            P.All_Values.Append (V);

            for Name of V.Names loop
               P.Name_To_Value.Insert (Name, V);
            end loop;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Property_Value_Aliases_Loader;
