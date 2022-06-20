--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Property_Aliases_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      Loader.Open (UCD_Root, "PropertyAliases.txt");

      while not Loader.End_Of_File loop
         declare
            P : constant Properties.Property_Access := new Properties.Property;

         begin
            P.Names.Append
              (To_Unbounded_Wide_Wide_String (Loader.Get_Field (0)));

            --  Second field is a long name of the property and may be the same
            --  as short name of the property, thus ignore it in such cases.

            declare
               Name : constant Unbounded_Wide_Wide_String :=
                 To_Unbounded_Wide_Wide_String (Loader.Get_Field (1));

            begin
               if Name /= P.Names.First_Element then
                  P.Names.Append (Name);
               end if;
            end;

            for J in 2 .. Data_File_Loaders.Field_Index'Last loop
               if Loader.Has_Field (J) then
                  P.Names.Append
                    (To_Unbounded_Wide_Wide_String (Loader.Get_Field (J)));
               end if;
            end loop;

            --  Compute some properties of the property.

            if P.Names.First_Element = "ccc" then
               P.Is_Canonical_Combining_Class := True;
            end if;

            --  Register property and its names.

            Properties.All_Properties.Append (P);

            for Name of P.Names loop
               Properties.Name_To_Property.Insert (Name, P);
            end loop;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Property_Aliases_Loader;
