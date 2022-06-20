--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Property_Value_Aliases_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropertyValueAliases.txt");

      while not Loader.End_Of_File loop
         --  XXX @missing annotation is not processed!

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

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Property_Value_Aliases_Loader;
