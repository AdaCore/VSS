--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Transformers.Casing;

separate (Test_Transformer)
procedure Test_Casing_Minimal is
   use type VSS.Strings.Virtual_String;

   S1 : constant VSS.Strings.Virtual_String := "123ABCАБВ";
   E1 : constant VSS.Strings.Virtual_String := "123abcабв";

   L1 : constant VSS.Strings.Virtual_String :=
     "abcdefghigklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя";
   U1 : constant VSS.Strings.Virtual_String :=
     "ABCDEFGHIGKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ";

   --  This is a testcase for Final_Sigma context. \u03A3 translated to \u03C3
   --  when there is no Final_Sigma context and to \u03C2 in Final_Sigma
   --  context.

   S1S : constant VSS.Strings.Virtual_String := "AΣZ";
   S1E : constant VSS.Strings.Virtual_String := "aσz";

   S2S : constant VSS.Strings.Virtual_String := "AΣ";
   S2E : constant VSS.Strings.Virtual_String := "aς";

   --  Single U+0345 ypogegrammeni is not satisfy Before condition of
   --  Final_Sigma context.

   S3S : constant VSS.Strings.Virtual_String := "ͅΣ";
   S3E : constant VSS.Strings.Virtual_String := "ͅσ";

   --  <capital-alpha, ypogegrammeni> satisfy Before condition of Final_Sigma
   --  context.

   S4S : constant VSS.Strings.Virtual_String := "ᾼΣ";
   S4E : constant VSS.Strings.Virtual_String := "ᾳς";

   --  Single ypogegrammeni satisfy After condition.

   S5S : constant VSS.Strings.Virtual_String := "ᾼΣͅ";
   S5E : constant VSS.Strings.Virtual_String := "ᾳςͅ";

   --  <ypogegrammeni, capital-alpha> doesn't satisfy After condition.

   S6S : constant VSS.Strings.Virtual_String := "ᾼΣͅΑ";
   S6E : constant VSS.Strings.Virtual_String := "ᾳσͅα";

   SN  : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Empty_Virtual_String;

begin
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (S1) = E1);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (S1S) = S1E);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (S2S) = S2E);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (S3S) = S3E);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (S4S) = S4E);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (S5S) = S5E);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (S6S) = S6E);

   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (U1) = L1);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Uppercase.Transform (L1) = U1);

   --  Test for null string for code coverage of Null_String_Handler.

   Test_Support.Assert
     (VSS.Transformers.Casing.To_Lowercase.Transform (SN) = SN);

   --  Test of simple case conversion.

   Test_Support.Assert
     (VSS.Transformers.Casing.To_Simple_Lowercase.Transform (S1) = E1);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Simple_Uppercase.Transform (E1) = S1);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Simple_Lowercase.Transform (U1) = L1);
   Test_Support.Assert
     (VSS.Transformers.Casing.To_Simple_Uppercase.Transform (L1) = U1);
end Test_Casing_Minimal;
