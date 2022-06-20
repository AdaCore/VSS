--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Emoji_Data_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      Name_Field : constant Data_File_Loaders.Field_Index := 1;
      --  Index of the data field with name of the property.

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      --  Unicode 13.0: exception: Extended_Pictographic property is Y by
      --  default for unassigned code points in few ranges.

      declare
         use type UCD.Properties.Property_Value_Access;

         GC_Property      : constant not null Properties.Property_Access :=
           Properties.Resolve ("gc");
         GC_Unassigned    : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (GC_Property, "Cn");
         ExtPict_Property : constant not null Properties.Property_Access :=
           Properties.Resolve ("ExtPict");
         ExtPict_Y        : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (ExtPict_Property, "Y");
         ExtPict_N        : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (ExtPict_Property, "N");

      begin
         for C in UCD.Code_Point loop
            if C in 16#01_F000# .. 16#01_FAFF# | 16#01_FC00# .. 16#01_FFFD#
            then
               if UCD.Characters.Get (C, GC_Property) = GC_Unassigned then
                  UCD.Characters.Set (C, ExtPict_Property, ExtPict_Y);

               else
                  UCD.Characters.Set (C, ExtPict_Property, ExtPict_N);
               end if;

            else
               UCD.Characters.Set (C, ExtPict_Property, ExtPict_N);
            end if;
         end loop;
      end;

      Loader.Open (UCD_Root, "emoji/emoji-data.txt");

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
                  Characters.Set
                    (Code, Property, Property.Name_To_Value.Element
                       (To_Unbounded_Wide_Wide_String ("Y")));
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Emoji_Data_Loader;
