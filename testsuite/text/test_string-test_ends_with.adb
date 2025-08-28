--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters.Latin;
with VSS.Characters.Punctuations;

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

   Test_Support.Assert (not SN1.Ends_With (' '));
   Test_Support.Assert (not SE1.Ends_With (' '));

   Test_Support.Assert (S1.Ends_With (' '));
   Test_Support.Assert (not S1.Ends_With ('z'));

   declare
      use type VSS.Strings.Line_Terminator;

      --  Line separator only

      Data_1 : constant
        array (VSS.Strings.Line_Terminator) of VSS.Strings.Virtual_String :=
        [VSS.Strings.CR =>
           VSS.Strings.To_Virtual_String
             ("" & Wide_Wide_Character (VSS.Characters.Latin.Carriage_Return)),
         VSS.Strings.LF =>
           VSS.Strings.To_Virtual_String
             ("" & Wide_Wide_Character (VSS.Characters.Latin.Line_Feed)),
         VSS.Strings.CRLF =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_Character (VSS.Characters.Latin.Carriage_Return)
                & Wide_Wide_Character (VSS.Characters.Latin.Line_Feed)),
         VSS.Strings.NEL =>
           VSS.Strings.To_Virtual_String
             ("" & Wide_Wide_Character (VSS.Characters.Latin.Next_Line)),
         VSS.Strings.VT =>
           VSS.Strings.To_Virtual_String
             ("" & Wide_Wide_Character (VSS.Characters.Latin.Line_Tabulation)),
         VSS.Strings.FF =>
           VSS.Strings.To_Virtual_String
             ("" & Wide_Wide_Character (VSS.Characters.Latin.Form_Feed)),
         VSS.Strings.LS =>
           VSS.Strings.To_Virtual_String
             ("" & Wide_Wide_Character
                     (VSS.Characters.Punctuations.Line_Separator)),
         VSS.Strings.PS =>
           VSS.Strings.To_Virtual_String
             ("" & Wide_Wide_Character
                     (VSS.Characters.Punctuations.Paragraph_Separator))];

      --  Some text and line separator

      Data_2 : constant
        array (VSS.Strings.Line_Terminator) of VSS.Strings.Virtual_String :=
        [VSS.Strings.CR =>
           VSS.Strings.To_Virtual_String
             ("a"
              & Wide_Wide_Character (VSS.Characters.Latin.Carriage_Return)),
         VSS.Strings.LF =>
           VSS.Strings.To_Virtual_String
             ("ba" & Wide_Wide_Character (VSS.Characters.Latin.Line_Feed)),
         VSS.Strings.CRLF =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_Character (VSS.Characters.Latin.Carriage_Return)
              & Wide_Wide_Character (VSS.Characters.Latin.Line_Feed)),
         VSS.Strings.NEL =>
           VSS.Strings.To_Virtual_String
             ("cba" & Wide_Wide_Character (VSS.Characters.Latin.Next_Line)),
         VSS.Strings.VT =>
           VSS.Strings.To_Virtual_String
             ("dcba"
              & Wide_Wide_Character (VSS.Characters.Latin.Line_Tabulation)),
         VSS.Strings.FF =>
           VSS.Strings.To_Virtual_String
             ("edcba" & Wide_Wide_Character (VSS.Characters.Latin.Form_Feed)),
         VSS.Strings.LS =>
           VSS.Strings.To_Virtual_String
             ("fedcba"
              & Wide_Wide_Character
                (VSS.Characters.Punctuations.Line_Separator)),
         VSS.Strings.PS =>
           VSS.Strings.To_Virtual_String
             ("gfedcba"
              & Wide_Wide_Character
                (VSS.Characters.Punctuations.Paragraph_Separator))];

   begin
      --  Null string never ends with any line terminator.

      for J in VSS.Strings.Line_Terminator'Range loop
         Test_Support.Assert (not SN1.Ends_With (J));
      end loop;

      --  Empty string never ends with any line terminator.

      for J in VSS.Strings.Line_Terminator'Range loop
         Test_Support.Assert (not SE1.Ends_With (J));
      end loop;

      --  String contains line terminator only.

      for J in VSS.Strings.Line_Terminator'Range loop
         for K in VSS.Strings.Line_Terminator'Range loop
            if J = K or else (J = VSS.Strings.CRLF and K = VSS.Strings.LF) then
               Test_Support.Assert (Data_1 (J).Ends_With (K));

            else
               Test_Support.Assert (not Data_1 (J).Ends_With (K));
            end if;
         end loop;
      end loop;

      --  String contains some text and ends with line terminator.

      for J in VSS.Strings.Line_Terminator'Range loop
         for K in VSS.Strings.Line_Terminator'Range loop
            if J = K or else (J = VSS.Strings.CRLF and K = VSS.Strings.LF) then
               Test_Support.Assert (Data_2 (J).Ends_With (K));

            else
               Test_Support.Assert (not Data_2 (J).Ends_With (K));
            end if;
         end loop;
      end loop;
   end;
end Test_Ends_With;
