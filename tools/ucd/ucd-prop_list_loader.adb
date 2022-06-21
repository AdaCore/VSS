--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Prop_List_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Name_Field : constant Data_File_Loaders.Field_Index := 1;
      --  Index of the data field with name of the property.

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropList.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : UCD.Code_Point;
            Last_Code  : UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            declare
               Property : constant not null Properties.Property_Access :=
                 Properties.Resolve (Loader.Get_Field (Name_Field));

            begin
               for Code in First_Code .. Last_Code loop
                  Characters.Set
                    (Code, Property, Property.Name_To_Value.Element
                       (To_Unbounded_Wide_Wide_String ("Y")));
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Prop_List_Loader;
