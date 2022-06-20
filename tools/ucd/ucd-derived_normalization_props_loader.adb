--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

with Ada.Wide_Wide_Text_IO;

package body UCD.Derived_Normalization_Props_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Name_Field  : constant Data_File_Loaders.Field_Index := 1;
      --  Index of the data field with name of the property.

      Value_Field : constant Data_File_Loaders.Field_Index := 2;
      --  Index of the data field for the value of property, when provided.

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      --  Default value for derived NFD_Quick_Check, NFC_Quick_Check,
      --  NFKD_Quick_Check properties is 'Y', this feature is not supported
      --  by the loader, thus set it manually.

      declare
         NFD_QC_Property  : constant not null Properties.Property_Access :=
           Properties.Resolve ("NFD_QC");
         NFD_QC_Y         : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (NFD_QC_Property, "Y");
         NFC_QC_Property  : constant not null Properties.Property_Access :=
           Properties.Resolve ("NFC_QC");
         NFC_QC_Y         : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (NFC_QC_Property, "Y");
         NFKD_QC_Property : constant not null Properties.Property_Access :=
           Properties.Resolve ("NFKD_QC");
         NFKD_QC_Y        : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (NFKD_QC_Property, "Y");
         NFKC_QC_Property : constant not null Properties.Property_Access :=
           Properties.Resolve ("NFKC_QC");
         NFKC_QC_Y        : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (NFKC_QC_Property, "Y");

      begin
         for Code in Code_Point loop
            Characters.Set (Code, NFD_QC_Property, NFD_QC_Y);
            Characters.Set (Code, NFC_QC_Property, NFC_QC_Y);
            Characters.Set (Code, NFKD_QC_Property, NFKD_QC_Y);
            Characters.Set (Code, NFKC_QC_Property, NFKC_QC_Y);
         end loop;
      end;

      Loader.Open (UCD_Root, "DerivedNormalizationProps.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : UCD.Code_Point;
            Last_Code  : UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            declare
               Property_Name : constant Wide_Wide_String :=
                 Loader.Get_Field (Name_Field);
               Property      : constant not null Properties.Property_Access :=
                 Properties.Resolve (Property_Name);

            begin
               if Property_Name
                    in "FC_NFKC" | "Expands_On_NFD" | "Expands_On_NFC"
                      | "Expands_On_NFKD" | "Expands_On_NFKC"
               then
                  --  FC_NFKC_Closure, Expands_On_NFD, Expands_On_NFC,
                  --  Expands_On_NFKD, Expands_On_NFKC properties are
                  --  deprecated and not used in VSS, don't load it.

                  null;

               elsif Property_Name
                       in "Full_Composition_Exclusion"
                         | "Changes_When_NFKC_Casefolded"
               then
                  --  Binary properties

                  for Code in First_Code .. Last_Code loop
                     Characters.Set
                       (Code, Property, Property.Name_To_Value.Element
                          (To_Unbounded_Wide_Wide_String ("Y")));
                  end loop;

               elsif Property_Name
                       in "NFD_QC" | "NFC_QC" | "NFKD_QC" | "NFKC_QC"
               then
                  --  Enumeration properties

                  for Code in First_Code .. Last_Code loop
                     Characters.Set
                       (Code,
                        Property,
                        Properties.Resolve
                          (Property, Loader.Get_Field (Value_Field)));
                  end loop;

               elsif Property_Name = "NFKC_CF" then
                  --  String property

                  Property.Is_String := True;

                  for Code in First_Code .. Last_Code loop
                     Characters.Set
                       (Code,
                        Property,
                        new Properties.Property_Value'
                          (Names                           => <>,
                           Is_Used                         => <>,
                           Canonical_Combining_Class_Value => <>,
                           String                          =>
                             Loader.Get_Field (Value_Field)));
                  end loop;

               else
                  Ada.Wide_Wide_Text_IO.Put_Line (''' & Property_Name & ''');

                  raise Program_Error;
               end if;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Derived_Normalization_Props_Loader;
