--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Special_Casing_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      --  Lowercase_Mapping, Titlecase_Mapping, and Uppercase_Mapping.

      LC_Field      : constant Data_File_Loaders.Field_Index := 1;
      LC_Property   : constant not null Properties.Property_Access :=
        Properties.Resolve ("lc");

      TC_Field      : constant Data_File_Loaders.Field_Index := 2;
      TC_Property   : constant not null Properties.Property_Access :=
        Properties.Resolve ("tc");

      UC_Field      : constant Data_File_Loaders.Field_Index := 3;
      UC_Property   : constant not null Properties.Property_Access :=
        Properties.Resolve ("uc");

      --  Context field

      Context_Field : constant Data_File_Loaders.Field_Index := 4;

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      --  Mark properties as string properties, this can't be determined
      --  during initial database construction.

      LC_Property.Is_String := True;
      TC_Property.Is_String := True;
      UC_Property.Is_String := True;

      Loader.Open (UCD_Root, "SpecialCasing.txt");

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
               use type Ada.Containers.Count_Type;

               LC_Data       : constant UCD.Code_Point_Vectors.Vector :=
                 Loader.Get_Field (LC_Field);
               LC_Value      :
                 constant not null Properties.Property_Value_Access :=
                 (if LC_Data.Length = 1 and then LC_Data.First_Element = Code
                  then null
                  else new Properties.Property_Value'
                    (Names                           => <>,
                     Is_Used                         => <>,
                     Canonical_Combining_Class_Value => <>,
                     String                          => LC_Data));
               TC_Data       : constant UCD.Code_Point_Vectors.Vector :=
                 Loader.Get_Field (TC_Field);
               TC_Value      :
                 constant not null Properties.Property_Value_Access :=
                 (if TC_Data.Length = 1 and then TC_Data.First_Element = Code
                  then null
                  else new Properties.Property_Value'
                    (Names                           => <>,
                     Is_Used                         => <>,
                     Canonical_Combining_Class_Value => <>,
                     String                          => TC_Data));
               UC_Data       : constant UCD.Code_Point_Vectors.Vector :=
                 Loader.Get_Field (UC_Field);
               UC_Value      :
                 constant not null Properties.Property_Value_Access :=
                 (if UC_Data.Length = 1 and then UC_Data.First_Element = Code
                  then null
                  else new Properties.Property_Value'
                    (Names                           => <>,
                     Is_Used                         => <>,
                     Canonical_Combining_Class_Value => <>,
                     String                          => UC_Data));
               Context_Value : constant Wide_Wide_String :=
                 Loader.Get_Field (Context_Field);

            begin
               if Context_Value = "" then
                  Characters.Set (Code, LC_Property, LC_Value);
                  Characters.Set (Code, TC_Property, TC_Value);
                  Characters.Set (Code, UC_Property, UC_Value);

               elsif Context_Value = "Final_Sigma" and Code = 16#03A3# then
                  --  This is single case of conditional mapping for default
                  --  locale. It may be hardcoded for now to simplify data
                  --  structures.

                  null;

               elsif Context_Value'Length >= 2
                 and then Context_Value
                   (Context_Value'First .. Context_Value'First + 1)
                      in "lt" | "tr" | "az"
               then
                  --  Ignore all locale specific mappings.

                  null;

               else
                  --  All other context specifications are not expected.

                  raise Program_Error;
               end if;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Special_Casing_Loader;
