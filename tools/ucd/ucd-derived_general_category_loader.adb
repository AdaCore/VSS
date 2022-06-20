--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Derived_General_Category_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      GC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("gc");

      Value_Field : constant Data_File_Loaders.Field_Index := 1;
      --  Index of the data field with the value of the property.

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "extracted/DerivedGeneralCategory.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : UCD.Code_Point;
            Last_Code  : UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            declare
               Value : constant not null Properties.Property_Value_Access :=
                 UCD.Properties.Resolve
                   (GC_Property, Loader.Get_Field (Value_Field));

            begin
               for Code in First_Code .. Last_Code loop
                  UCD.Characters.Set (Code, GC_Property, Value);
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Derived_General_Category_Loader;
