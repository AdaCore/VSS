--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with VSS.Characters;
with VSS.Strings;

with UCD.Data_File_Loaders;

with Test_Support;

procedure Test_String_Normalization is

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

   UCD_Root : constant Wide_Wide_String :=
     Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
       (Ada.Command_Line.Argument (1));

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
                    C1.To_Normalized (VSS.Strings.Normalization_Form_C);
                  C2N : constant VSS.Strings.Virtual_String :=
                    C2.To_Normalized (VSS.Strings.Normalization_Form_C);
                  C3N : constant VSS.Strings.Virtual_String :=
                    C3.To_Normalized (VSS.Strings.Normalization_Form_C);
                  C4N : constant VSS.Strings.Virtual_String :=
                    C4.To_Normalized (VSS.Strings.Normalization_Form_C);
                  C5N : constant VSS.Strings.Virtual_String :=
                    C5.To_Normalized (VSS.Strings.Normalization_Form_C);

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
                    C1.To_Normalized (VSS.Strings.Normalization_Form_D);
                  C2N : constant VSS.Strings.Virtual_String :=
                    C2.To_Normalized (VSS.Strings.Normalization_Form_D);
                  C3N : constant VSS.Strings.Virtual_String :=
                    C3.To_Normalized (VSS.Strings.Normalization_Form_D);
                  C4N : constant VSS.Strings.Virtual_String :=
                    C4.To_Normalized (VSS.Strings.Normalization_Form_D);
                  C5N : constant VSS.Strings.Virtual_String :=
                    C5.To_Normalized (VSS.Strings.Normalization_Form_D);

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
                    C1.To_Normalized (VSS.Strings.Normalization_Form_KC);
                  C2N : constant VSS.Strings.Virtual_String :=
                    C2.To_Normalized (VSS.Strings.Normalization_Form_KC);
                  C3N : constant VSS.Strings.Virtual_String :=
                    C3.To_Normalized (VSS.Strings.Normalization_Form_KC);
                  C4N : constant VSS.Strings.Virtual_String :=
                    C4.To_Normalized (VSS.Strings.Normalization_Form_KC);
                  C5N : constant VSS.Strings.Virtual_String :=
                    C5.To_Normalized (VSS.Strings.Normalization_Form_KC);

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
                    C1.To_Normalized (VSS.Strings.Normalization_Form_KD);
                  C2N : constant VSS.Strings.Virtual_String :=
                    C2.To_Normalized (VSS.Strings.Normalization_Form_KD);
                  C3N : constant VSS.Strings.Virtual_String :=
                    C3.To_Normalized (VSS.Strings.Normalization_Form_KD);
                  C4N : constant VSS.Strings.Virtual_String :=
                    C4.To_Normalized (VSS.Strings.Normalization_Form_KD);
                  C5N : constant VSS.Strings.Virtual_String :=
                    C5.To_Normalized (VSS.Strings.Normalization_Form_KD);

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
end Test_String_Normalization;
