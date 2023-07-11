--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

separate (Test_String)
procedure Test_Ends_With is

   SE1 : constant VSS.Strings.Virtual_String := "";
   SE2 : constant VSS.Strings.Virtual_String := "";

   SN1 : VSS.Strings.Virtual_String;
   pragma Warnings (Off, SN1);
   SN2 : VSS.Strings.Virtual_String;
   pragma Warnings (Off, SN2);

   S1 : constant VSS.Strings.Virtual_String := "ASCII Кириллица ⊗∬ 𝛻𝜕 ";

   Prefix_1 : constant VSS.Strings.Virtual_String := "ASCII";
   Prefix_2 : constant VSS.Strings.Virtual_String := "Кириллица";
   Suffix_1 : constant VSS.Strings.Virtual_String := "𝜕 ";

begin
   Test_Support.Assert (SN1.Ends_With (SN2));
   Test_Support.Assert (SN1.Ends_With (SE1));

   Test_Support.Assert (not SN1.Ends_With (Prefix_1));
   Test_Support.Assert (not SN1.Ends_With (Prefix_2));

   Test_Support.Assert (SE1.Ends_With (SN1));

   Test_Support.Assert (SE1.Ends_With (SE1));
   Test_Support.Assert (SE1.Ends_With (SE2));

   Test_Support.Assert (S1.Ends_With (SN1));
   Test_Support.Assert (S1.Ends_With (SE1));

   Test_Support.Assert (S1.Ends_With (Suffix_1));
   Test_Support.Assert (not S1.Ends_With (Prefix_2));

   Test_Support.Assert (not Prefix_1.Ends_With (S1));
   Test_Support.Assert (not Prefix_2.Ends_With (S1));
end Test_Ends_With;
