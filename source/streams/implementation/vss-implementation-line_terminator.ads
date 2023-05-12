--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Constants for line termination sequences.

with VSS.Characters.Latin;
with VSS.Characters.Punctuations;
with VSS.Strings;

package VSS.Implementation.Line_Terminator is

   Sequence : constant
     array (VSS.Strings.Line_Terminator) of VSS.Strings.Virtual_String
     := [VSS.Strings.CR =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character
                          (VSS.Characters.Latin.Carriage_Return))),
         VSS.Strings.LF =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character (VSS.Characters.Latin.Line_Feed))),
         VSS.Strings.CRLF =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character
                          (VSS.Characters.Latin.Carriage_Return),
                   2 => Wide_Wide_Character (VSS.Characters.Latin.Line_Feed))),
         VSS.Strings.NEL =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character (VSS.Characters.Latin.Next_Line))),
         VSS.Strings.VT =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character
                          (VSS.Characters.Latin.Line_Tabulation))),
         VSS.Strings.FF =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character (VSS.Characters.Latin.Form_Feed))),
         VSS.Strings.LS =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character
                          (VSS.Characters.Punctuations.Line_Separator))),
         VSS.Strings.PS =>
           VSS.Strings.To_Virtual_String
             (Wide_Wide_String'
                  (1 => Wide_Wide_Character
                          (VSS.Characters.Punctuations.Paragraph_Separator)))];

end VSS.Implementation.Line_Terminator;
