--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body UCD.Property_Value_Aliases_Loader is

   ------------------
   -- Load_Aliases --
   ------------------

   procedure Load_Aliases (UCD_Root : Wide_Wide_String) is
      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropertyValueAliases.txt", "aliases");

      while not Loader.End_Of_File loop
         if not Loader.Is_Missing then
            declare
               P : constant Properties.Property_Access :=
                 Properties.Name_To_Property
                   (To_Unbounded_Wide_Wide_String (Loader.Get_Field (0)));
               V : constant Properties.Property_Value_Access :=
                 new Properties.Property_Value;
               F : Data_File_Loaders.Field_Index := 1;

            begin
               if P.Is_Canonical_Combining_Class then
                  --  Second field for 'ccc' property is numeric value.

                  V.Canonical_Combining_Class_Value :=
                    Properties.Canonical_Combinig_Class'Wide_Wide_Value
                      (Loader.Get_Field (1));
                  F := 2;
               end if;

               for J in F .. Data_File_Loaders.Field_Index'Last loop
                  if Loader.Has_Field (J) then
                     declare
                        Name : constant Unbounded_Wide_Wide_String :=
                          To_Unbounded_Wide_Wide_String (Loader.Get_Field (J));

                     begin
                        --  Short name and long name may be the same, ignore
                        --  duplicates.

                        if V.Names.Is_Empty
                             or else Name /= V.Names.First_Element
                        then
                           V.Names.Append (Name);
                        end if;
                     end;
                  end if;
               end loop;

               --  Register value and its names

               P.All_Values.Append (V);

               for Name of V.Names loop
                  P.Name_To_Value.Insert (Name, V);
               end loop;

            end;

         end if;

         Loader.Skip_Line;
      end loop;
   end Load_Aliases;

   ------------------
   -- Load_Missing --
   ------------------

   procedure Load_Missing (UCD_Root : Wide_Wide_String) is
      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropertyValueAliases.txt", "missing");

      while not Loader.End_Of_File loop
         if Loader.Is_Missing then
            declare
               use type Properties.Property_Value_Access;

               First_Code : UCD.Code_Point;
               Last_Code  : UCD.Code_Point;

               Property   : constant Properties.Property_Access :=
                 Properties.Name_To_Property
                   (To_Unbounded_Wide_Wide_String
                      (Loader.Get_Field (1, True)));
               Image      : constant Wide_Wide_String :=
                 Loader.Get_Field (2, True);
               Value      : constant Properties.Property_Value_Access :=
                 (if Image in "<none>" | "<code point>" | "NaN"
                    then null else Properties.Resolve (Property, Image));
               --  Ignore:
               --    - "<none>" for string properties;
               --    - "<code point>" for mapping to itself;
               --    - "NaN" for numeric values.

            begin
               Loader.Get_Code_Point_Range (First_Code, Last_Code);

               --  Put_Line (Image);

               if Value /= null then
                  Ada.Wide_Wide_Text_IO.Put_Line
                    ("  - set " & Loader.Get_Field (1, True)
                     & " to '" & Loader.Get_Field (2, True)
                     & "' in " & Loader.Get_Field (0, True));

                  for Code in First_Code .. Last_Code loop
                     Characters.Set (Code, Property, Value);
                  end loop;
               end if;
            end;
         end if;

         Loader.Skip_Line;
      end loop;
   end Load_Missing;

end UCD.Property_Value_Aliases_Loader;
