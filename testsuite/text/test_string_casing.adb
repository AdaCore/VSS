--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

with Test_Support;

procedure Test_String_Casing is
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
   Test_Support.Assert (S1.To_Lowercase = E1);
   Test_Support.Assert (S1S.To_Lowercase = S1E);
   Test_Support.Assert (S2S.To_Lowercase = S2E);
   Test_Support.Assert (S3S.To_Lowercase = S3E);
   Test_Support.Assert (S4S.To_Lowercase = S4E);
   Test_Support.Assert (S5S.To_Lowercase = S5E);
   Test_Support.Assert (S6S.To_Lowercase = S6E);

   Test_Support.Assert (U1.To_Lowercase = L1);
   Test_Support.Assert (L1.To_Uppercase = U1);

   --  Test for null string for code coverage of Null_String_Handler.

   Test_Support.Assert (SN.To_Lowercase = SN);

   --  Test of simple case conversion.

   Test_Support.Assert (S1.To_Simple_Lowercase = E1);
   Test_Support.Assert (E1.To_Simple_Uppercase = S1);
   Test_Support.Assert (U1.To_Simple_Lowercase = L1);
   Test_Support.Assert (L1.To_Simple_Uppercase = U1);
end Test_String_Casing;
