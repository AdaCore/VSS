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
      Data    : VSS.Implementation.Strings.String_Data;
      Offset  : VSS.Implementation.Strings.Cursor_Offset;
      Handler : VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
      case Terminator is
         when VSS.Strings.CR =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Carriage_Return,
               Offset);

         when VSS.Strings.LF =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Feed,
               Offset);

         when VSS.Strings.CRLF =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Carriage_Return,
               Offset);
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Feed,
               Offset);

         when VSS.Strings.NEL =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Next_Line,
               Offset);

         when VSS.Strings.VT =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Tabulation,
               Offset);

         when VSS.Strings.FF =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Form_Feed,
               Offset);

         when VSS.Strings.LS =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
              (Data,
               VSS.Implementation.Character_Codes.Line_Separator,
               Offset);

         when VSS.Strings.PS =>
            Handler := VSS.Implementation.Strings.Variable_Handler (Data);
            Handler.Append
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
