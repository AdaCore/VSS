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

with Gen_UCD.Characters;
with Gen_UCD.Data_File_Loaders;
with Gen_UCD.Properties;

package body Gen_UCD.Unicode_Data_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      --  General_Category

      GC_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("gc");
      GC_Field    : constant Data_File_Loaders.Field_Index := 2;

      --  Canonical_Combinig_Class

      CCC_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("ccc");
      CCC_Field    : constant Data_File_Loaders.Field_Index := 3;

      Loader : Gen_UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "UnicodeData.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : Gen_UCD.Code_Point;
            Last_Code  : Gen_UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            declare
               GC_Value  :
                 constant not null Properties.Property_Value_Access :=
                   Properties.Resolve
                     (GC_Property, Loader.Get_Field (GC_Field));
               CCC_Value :
                 constant not null Properties.Property_Value_Access :=
                   Properties.Resolve
                     (CCC_Property, Loader.Get_Field (CCC_Field));

            begin
               for Code in First_Code .. Last_Code loop
                  Characters.Set (Code, GC_Property, GC_Value);
                  Characters.Set (Code, CCC_Property, CCC_Value);
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end Gen_UCD.Unicode_Data_Loader;
