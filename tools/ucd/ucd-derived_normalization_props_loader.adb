--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Text_IO;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

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
      Loader.Open (UCD_Root, "DerivedNormalizationProps.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : UCD.Code_Point;
            Last_Code  : UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            if Loader.Is_Missing then
               Ada.Wide_Wide_Text_IO.Put_Line
                 ("  - set " & Loader.Get_Field (1, True)
                  & " to '" & Loader.Get_Field (2, True)
                  & "' in " & Loader.Get_Field (0, True));
            end if;

            declare
               Property_Name : constant Wide_Wide_String :=
                 Loader.Get_Field (Name_Field, True);
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
                          (Property, Loader.Get_Field (Value_Field, True)));
                  end loop;

               elsif Property_Name in "NFKC_CF" | "NFKC_SCF" then
                  --  String property

                  Property.Is_String := True;

                  if Loader.Get_Field (Value_Field, True)
                    /= "<code point>"
                  then
                     for Code in First_Code .. Last_Code loop
                        Characters.Set
                          (Code,
                           Property,
                           new Properties.Property_Value'
                             (Names                           => <>,
                              Is_Used                         => <>,
                              Canonical_Combining_Class_Value => <>,
                              String                          =>
                                Loader.Get_Field (Value_Field, True)));
                     end loop;
                  end if;

               else
                  raise Program_Error;
               end if;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Derived_Normalization_Props_Loader;
