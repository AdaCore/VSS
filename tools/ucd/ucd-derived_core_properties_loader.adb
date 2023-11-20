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
      --  Load data

      Loader.Open (UCD_Root, "DerivedCoreProperties.txt");

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
               Property : constant not null Properties.Property_Access :=
                 Properties.Resolve (Loader.Get_Field (Name_Field, True));

            begin
               for Code in First_Code .. Last_Code loop
                  if Loader.Has_Field (Value_Field) then
                     --  Unicode 15.1: property value is specified

                     Characters.Set
                       (Code,
                        Property,
                        Property.Name_To_Value.Element
                          (To_Unbounded_Wide_Wide_String
                               (Loader.Get_Field (Value_Field, True))));

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
