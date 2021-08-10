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
end Test_String_Casing;
