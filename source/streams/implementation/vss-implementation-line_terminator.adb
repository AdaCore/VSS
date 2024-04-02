--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Character_Codes;
with VSS.Implementation.Strings;
with VSS.Implementation.Text_Handlers;
with VSS.Strings.Internals;

package body VSS.Implementation.Line_Terminator is

   --------------
   -- Sequence --
   --------------

   function Sequence
     (Terminator : VSS.Strings.Line_Terminator)
      return VSS.Strings.Virtual_String
   is
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      return Result : VSS.Strings.Virtual_String do
         declare
            Text : constant not null
              VSS.Implementation.Strings.Variable_Text_Handler_Access :=
                VSS.Implementation.Strings.Variable_Handler
                  (VSS.Strings.Internals.Data_Access_Variable (Result).all);

         begin
            case Terminator is
               when VSS.Strings.CR =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Carriage_Return,
                     Offset);

               when VSS.Strings.LF =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Line_Feed, Offset);

               when VSS.Strings.CRLF =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Carriage_Return,
                     Offset);
                  Text.Append
                    (VSS.Implementation.Character_Codes.Line_Feed, Offset);

               when VSS.Strings.NEL =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Next_Line, Offset);

               when VSS.Strings.VT =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Line_Tabulation,
                     Offset);

               when VSS.Strings.FF =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Form_Feed, Offset);

               when VSS.Strings.LS =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Line_Separator,
                     Offset);

               when VSS.Strings.PS =>
                  Text.Append
                    (VSS.Implementation.Character_Codes.Paragraph_Separator,
                     Offset);
            end case;
         end;
      end return;
   end Sequence;

end VSS.Implementation.Line_Terminator;
