--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Character_Codes;
with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;
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
            Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data
              renames VSS.Strings.Internals.Data_Access_Variable (Result).all;

         begin
            case Terminator is
               when VSS.Strings.CR =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Carriage_Return,
                     Offset);

               when VSS.Strings.LF =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Line_Feed,
                     Offset);

               when VSS.Strings.CRLF =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Carriage_Return,
                     Offset);
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Line_Feed,
                     Offset);

               when VSS.Strings.NEL =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Next_Line,
                     Offset);

               when VSS.Strings.VT =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Line_Tabulation,
                     Offset);

               when VSS.Strings.FF =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Form_Feed,
                     Offset);

               when VSS.Strings.LS =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Line_Separator,
                     Offset);

               when VSS.Strings.PS =>
                  VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                    (Text,
                     VSS.Implementation.Character_Codes.Paragraph_Separator,
                     Offset);
            end case;
         end;
      end return;
   end Sequence;

end VSS.Implementation.Line_Terminator;
