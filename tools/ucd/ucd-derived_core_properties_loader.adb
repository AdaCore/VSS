--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Derived_Core_Properties_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Name_Field  : constant Data_File_Loaders.Field_Index := 1;
      --  Index of the data field with name of the property.
      Value_Field : constant Data_File_Loaders.Field_Index := 2;
      --  Index of the data field with the value of the property.

      Loader      : UCD.Data_File_Loaders.File_Loader;

   begin
      --  Unicode 15.1: Setup default value of Indic_Conjunct_Break
      --
      --  XXX Default value can be constructed from @missing line

      declare
         InCB_Property : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("InCB");
         InCB_None     :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (InCB_Property, "None");

      begin
         --  Setup default value for all characters.

         InCB_None.Is_Used := True;

         for Code in UCD.Code_Point loop
            UCD.Characters.Set (Code, InCB_Property, InCB_None);
         end loop;
      end;

      --  Load data

      Loader.Open (UCD_Root, "DerivedCoreProperties.txt");

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
                  if Loader.Has_Field (Value_Field) then
                     --  Unicode 15.1: property value is specified

                     Characters.Set
                       (Code,
                        Property,
                        Property.Name_To_Value.Element
                          (To_Unbounded_Wide_Wide_String
                               (Loader.Get_Field (Value_Field))));

                  else
                     --  Only property name is specified, value assumed as "Y"

                     Characters.Set
                       (Code, Property, Property.Name_To_Value.Element
                          (To_Unbounded_Wide_Wide_String ("Y")));
                  end if;
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Derived_Core_Properties_Loader;
