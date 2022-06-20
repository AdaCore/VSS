--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with UCD.Characters;
with UCD.Data_File_Loaders;
with UCD.Properties;

package body UCD.Unicode_Data_Loader is

   function Lookup_Decomposition_Type
     (Tag : Unbounded_Wide_Wide_String)
      return UCD.Properties.Property_Value_Access;

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

      --  Decomposition properties

      DTM_Field   : constant Data_File_Loaders.Field_Index := 5;
      DT_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("dt");
      DM_Property : constant not null Properties.Property_Access :=
        Properties.Resolve ("dm");

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
      DM_Property.Is_String  := True;

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

               DT_Image : Unbounded_Wide_Wide_String;
               DT_Value : UCD.Properties.Property_Value_Access;
               DM_Data  : UCD.Code_Point_Vectors.Vector;

            begin
               Loader.Get_Field (DTM_Field, DT_Image, DM_Data);
               DT_Value := Lookup_Decomposition_Type (DT_Image);

               for Code in First_Code .. Last_Code loop
                  UCD.Characters.Set (Code, GC_Property, GC_Value);
                  UCD.Characters.Set (Code, CCC_Property, CCC_Value);

                  if not DM_Data.Is_Empty then
                     UCD.Characters.Set (Code, DT_Property, DT_Value);
                     UCD.Characters.Set
                       (Code,
                        DM_Property,
                        new UCD.Properties.Property_Value'
                          (Names                           => <>,
                           Is_Used                         => <>,
                           Canonical_Combining_Class_Value => <>,
                           String                          => DM_Data));
                  end if;

                  UCD.Characters.Set (Code, SUC_Property, SUC_Value);
                  UCD.Characters.Set (Code, SLC_Property, SLC_Value);
                  UCD.Characters.Set (Code, STC_Property, STC_Value);
               end loop;
            end;

            Loader.Skip_Line;
         end;
      end loop;
   end Load;

   -------------------------------
   -- Lookup_Decomposition_Type --
   -------------------------------

   function Lookup_Decomposition_Type
     (Tag : Unbounded_Wide_Wide_String)
      return UCD.Properties.Property_Value_Access
   is
      DT_Property  : constant not null Properties.Property_Access :=
        Properties.Resolve ("dt");

   begin
      if Tag = "" then
         return UCD.Properties.Resolve (DT_Property, "Canonical");

      elsif Tag = "<font>" then
         return UCD.Properties.Resolve (DT_Property, "Font");

      elsif Tag = "<noBreak>" then
         return UCD.Properties.Resolve (DT_Property, "Nobreak");

      elsif Tag = "<initial>" then
         return UCD.Properties.Resolve (DT_Property, "Initial");

      elsif Tag = "<medial>" then
         return UCD.Properties.Resolve (DT_Property, "Medial");

      elsif Tag = "<final>" then
         return UCD.Properties.Resolve (DT_Property, "Final");

      elsif Tag = "<isolated>" then
         return UCD.Properties.Resolve (DT_Property, "Isolated");

      elsif Tag = "<circle>" then
         return UCD.Properties.Resolve (DT_Property, "Circle");

      elsif Tag = "<super>" then
         return UCD.Properties.Resolve (DT_Property, "Super");

      elsif Tag = "<sub>" then
         return UCD.Properties.Resolve (DT_Property, "Sub");

      elsif Tag = "<vertical>" then
         return UCD.Properties.Resolve (DT_Property, "Vertical");

      elsif Tag = "<wide>" then
         return UCD.Properties.Resolve (DT_Property, "Wide");

      elsif Tag = "<narrow>" then
         return UCD.Properties.Resolve (DT_Property, "Narrow");

      elsif Tag = "<small>" then
         return UCD.Properties.Resolve (DT_Property, "Small");

      elsif Tag = "<square>" then
         return UCD.Properties.Resolve (DT_Property, "Square");

      elsif Tag = "<fraction>" then
         return UCD.Properties.Resolve (DT_Property, "Fraction");

      elsif Tag = "<compat>" then
         return UCD.Properties.Resolve (DT_Property, "Compat");

      else
         raise Program_Error;
      end if;
   end Lookup_Decomposition_Type;

end UCD.Unicode_Data_Loader;
