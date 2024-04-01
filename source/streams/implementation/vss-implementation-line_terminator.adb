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
      Data   : VSS.Implementation.Strings.String_Data;
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      case Terminator is
         when VSS.Strings.CR =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Carriage_Return,
               Offset);

         when VSS.Strings.LF =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Feed,
               Offset);

         when VSS.Strings.CRLF =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Carriage_Return,
               Offset);
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Feed,
               Offset);

         when VSS.Strings.NEL =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Next_Line,
               Offset);

         when VSS.Strings.VT =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Tabulation,
               Offset);

         when VSS.Strings.FF =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Form_Feed,
               Offset);

         when VSS.Strings.LS =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Separator,
               Offset);

         when VSS.Strings.PS =>
            VSS.Implementation.Strings.Variable_Handler (Data).Append
              (Data,
               VSS.Implementation.Character_Codes.Paragraph_Separator,
               Offset);
      end case;

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Data)
      do
         VSS.Implementation.Strings.Unreference (Data);
      end return;
   end Sequence;

end VSS.Implementation.Line_Terminator;
