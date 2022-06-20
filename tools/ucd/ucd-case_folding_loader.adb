--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Case_Folding_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      --  Case_Folding, Simple_Case_Folding properties.

      SCF_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("scf");
      CF_Property  : constant not null Properties.Property_Access :=
        Properties.Resolve ("cf");

      --  <status> and <mapping> fields.

      Status_Field  : constant Data_File_Loaders.Field_Index := 1;
      Mapping_Field : constant Data_File_Loaders.Field_Index := 2;

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      --  Mark properties as string properties, this can't be determined
      --  during initial database construction.

      SCF_Property.Is_String := True;
      CF_Property.Is_String  := True;

      Loader.Open (UCD_Root, "CaseFolding.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : UCD.Code_Point;
            Last_Code  : UCD.Code_Point;
            Code       : UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            if First_Code /= Last_Code then
               raise Program_Error;
            end if;

            Code := First_Code;

            declare
               Mapping_Data  : constant UCD.Code_Point_Vectors.Vector :=
                 Loader.Get_Field (Mapping_Field);
               Mapping_Value :
                 constant not null Properties.Property_Value_Access :=
                   new Properties.Property_Value'
                    (Names                           => <>,
                     Is_Used                         => <>,
                     Canonical_Combining_Class_Value => <>,
                     String                          => Mapping_Data);
               Status_Value  : constant Wide_Wide_String :=
                 Loader.Get_Field (Status_Field);

            begin
               if Status_Value = "C" then
                  Characters.Set (Code, SCF_Property, Mapping_Value);
                  Characters.Set (Code, CF_Property, Mapping_Value);

               elsif Status_Value = "F" then
                  Characters.Set (Code, CF_Property, Mapping_Value);

               elsif Status_Value = "S" then
                  Characters.Set (Code, SCF_Property, Mapping_Value);

               elsif Status_Value = "T" then
                  --  Case folding mapping for Turkic language is ignored.

                  null;

               else
                  raise Program_Error;
               end if;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Case_Folding_Loader;
