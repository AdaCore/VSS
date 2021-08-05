------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Unicode_Data_Loader is

   ----------
   -- Load --
   ----------

   procedure Load (UCD_Root : Wide_Wide_String) is
      --  General_Category

      GC_Field    : constant Data_File_Loaders.Field_Index := 2;
      GC_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("gc");

      --  Canonical_Combinig_Class

      CCC_Field    : constant Data_File_Loaders.Field_Index := 3;
      CCC_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("ccc");

      --  Simple_Uppercase_Mapping, Simple_Lowercase_Mapping, and
      --  Simple_Titlecase_Mapping.

      SUC_Field    : constant Data_File_Loaders.Field_Index := 12;
      SUC_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("suc");

      SLC_Field    : constant Data_File_Loaders.Field_Index := 13;
      SLC_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("slc");

      STC_Field    : constant Data_File_Loaders.Field_Index := 14;
      STC_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("stc");

      Loader : UCD.Data_File_Loaders.File_Loader;

   begin
      --  Mark properties as string properties, this can't be determined
      --  during initial database construction.

      SUC_Property.Is_String := True;
      SLC_Property.Is_String := True;
      STC_Property.Is_String := True;

      Loader.Open (UCD_Root, "UnicodeData.txt");

      while not Loader.End_Of_File loop
         declare
            First_Code : UCD.Code_Point;
            Last_Code  : UCD.Code_Point;

         begin
            Loader.Get_Code_Point_Range (First_Code, Last_Code);

            declare
               GC_Value  :
                 constant not null Properties.Property_Value_Access :=
                   Properties.Resolve
                     (GC_Property, Loader.Get_Field (GC_Field));
               CCC_Value :
                 constant not null Properties.Property_Value_Access :=
                   Properties.Resolve
                     (CCC_Property, Loader.Get_Field (CCC_Field));
               SUC_Data  : constant UCD.Code_Point_Vectors.Vector :=
                 Loader.Get_Field (SUC_Field);
               SUC_Value :
                 constant not null Properties.Property_Value_Access :=
                 (if SUC_Data.Is_Empty
                  then null
                  else new Properties.Property_Value'
                    (Names                           => <>,
                     Is_Used                         => <>,
                     Canonical_Combining_Class_Value => <>,
                     String                          => SUC_Data));
               SLC_Data  : constant UCD.Code_Point_Vectors.Vector :=
                 Loader.Get_Field (SLC_Field);
               SLC_Value :
                 constant not null Properties.Property_Value_Access :=
                 (if SLC_Data.Is_Empty
                  then null
                  else new Properties.Property_Value'
                    (Names                           => <>,
                     Is_Used                         => <>,
                     Canonical_Combining_Class_Value => <>,
                     String                          => SLC_Data));
               STC_Data : constant UCD.Code_Point_Vectors.Vector :=
                 Loader.Get_Field (STC_Field);
               STC_Value :
                 constant not null Properties.Property_Value_Access :=
                 (if STC_Data.Is_Empty
                  then null
                  else new Properties.Property_Value'
                    (Names                           => <>,
                     Is_Used                         => <>,
                     Canonical_Combining_Class_Value => <>,
                     String                          => STC_Data));

            begin
               for Code in First_Code .. Last_Code loop
                  Characters.Set (Code, GC_Property, GC_Value);
                  Characters.Set (Code, CCC_Property, CCC_Value);

                  Characters.Set (Code, SUC_Property, SUC_Value);
                  Characters.Set (Code, SLC_Property, SLC_Value);
                  Characters.Set (Code, STC_Property, STC_Value);
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

end UCD.Unicode_Data_Loader;
