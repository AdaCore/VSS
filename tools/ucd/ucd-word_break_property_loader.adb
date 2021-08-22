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

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Word_Break_Property_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      WB_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("WB");
      GCB_Other   : constant not null UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (WB_Property, "Other");
      Value_Field : constant Data_File_Loaders.Field_Index := 1;
      --  Index of the data field with the value of the property.

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      --  Setup default value for all characters.

      for Code in UCD.Code_Point loop
         UCD.Characters.Set (Code, WB_Property, GCB_Other);
      end loop;

      Loader.Open (UCD_Root, "auxiliary/WordBreakProperty.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : UCD.Code_Point;
            Last_Code  : UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            declare
               Value : constant not null Properties.Property_Value_Access :=
                 UCD.Properties.Resolve
                   (WB_Property, Loader.Get_Field (Value_Field));

            begin
               for Code in First_Code .. Last_Code loop
                  UCD.Characters.Set (Code, WB_Property, Value);
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Word_Break_Property_Loader;
