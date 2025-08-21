--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters;
with VSS.Transformers.Normalization;

with UCD.Data_File_Loaders;

separate (Test_Transformer)
procedure Test_UCD_NormalizationTest is

   function Get_Field
     (Loader : UCD.Data_File_Loaders.File_Loader'Class;
      Index  : UCD.Data_File_Loaders.Field_Index)
      return VSS.Strings.Virtual_String;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Loader : UCD.Data_File_Loaders.File_Loader'Class;
      Index  : UCD.Data_File_Loaders.Field_Index)
      return VSS.Strings.Virtual_String
   is
      Data : constant UCD.Code_Point_Vectors.Vector :=
        Loader.Get_Field (Index);

   begin
      return Result : VSS.Strings.Virtual_String do
         for Code of Data loop
            Result.Append (VSS.Characters.Virtual_Character'Val (Code));
         end loop;
      end return;
   end Get_Field;

   Loader : UCD.Data_File_Loaders.File_Loader;

begin
   Loader.Open (UCD_Root, "NormalizationTest.txt");

   while not Loader.End_Of_File loop
      declare
         use type VSS.Strings.Virtual_String;

         F0 : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String (Loader.Get_Field (0));

      begin
         if F0.Starts_With ("@Part") then
            null;

         else
            declare
               C1 : constant VSS.Strings.Virtual_String :=
                 Get_Field (Loader, 0);
               C2 : constant VSS.Strings.Virtual_String :=
                 Get_Field (Loader, 1);
               C3 : constant VSS.Strings.Virtual_String :=
                 Get_Field (Loader, 2);
               C4 : constant VSS.Strings.Virtual_String :=
                 Get_Field (Loader, 3);
               C5 : constant VSS.Strings.Virtual_String :=
                 Get_Field (Loader, 4);

            begin
               --  NFC

               declare
                  C1N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_C
                      .Transform (C1);
                  C2N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_C
                      .Transform (C2);
                  C3N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_C
                      .Transform (C3);
                  C4N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_C
                      .Transform (C4);
                  C5N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_C
                      .Transform (C4);

               begin
                  Test_Support.Assert (C2 = C1N);
                  Test_Support.Assert (C2 = C2N);
                  Test_Support.Assert (C2 = C3N);
                  Test_Support.Assert (C4 = C4N);
                  Test_Support.Assert (C4 = C5N);
               end;

               --  NFD

               declare
                  C1N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_D
                      .Transform (C1);
                  C2N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_D
                      .Transform (C2);
                  C3N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_D
                      .Transform (C3);
                  C4N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_D
                      .Transform (C4);
                  C5N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_D
                      .Transform (C5);

               begin
                  Test_Support.Assert (C3 = C1N);
                  Test_Support.Assert (C3 = C2N);
                  Test_Support.Assert (C3 = C3N);
                  Test_Support.Assert (C5 = C4N);
                  Test_Support.Assert (C5 = C5N);
               end;

               --  NFKC

               declare
                  C1N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KC
                      .Transform (C1);
                  C2N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KC
                      .Transform (C2);
                  C3N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KC
                      .Transform (C3);
                  C4N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KC
                      .Transform (C4);
                  C5N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KC
                      .Transform (C5);

               begin
                  Test_Support.Assert (C4 = C1N);
                  Test_Support.Assert (C4 = C2N);
                  Test_Support.Assert (C4 = C3N);
                  Test_Support.Assert (C4 = C4N);
                  Test_Support.Assert (C4 = C5N);
               end;

               --  NFKD

               declare
                  C1N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KD
                      .Transform (C1);
                  C2N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KD
                      .Transform (C2);
                  C3N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KD
                      .Transform (C3);
                  C4N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KD
                      .Transform (C4);
                  C5N : constant VSS.Strings.Virtual_String :=
                    VSS.Transformers.Normalization.To_Normalization_Form_KD
                      .Transform (C5);

               begin
                  Test_Support.Assert (C5 = C1N);
                  Test_Support.Assert (C5 = C2N);
                  Test_Support.Assert (C5 = C3N);
                  Test_Support.Assert (C5 = C4N);
                  Test_Support.Assert (C5 = C5N);
               end;

            end;
         end if;

         Loader.Skip_Line;
      end;
   end loop;

   Loader.Close;
end Test_UCD_NormalizationTest;
