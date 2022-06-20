--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Handlers;

package body VSS.Strings.Cursors.Iterators.Lines.Internals is

   ----------------
   -- First_Line --
   ----------------

   function First_Line
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator
   is
      Handler  :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      Position : VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Lines.Line_Iterator
      do
         Handler.Before_First_Character (Self.Data, Position);
         Dummy := Handler.Forward (Self.Data, Position);
         Result.Initialize (Self, Position, Terminators, Keep_Terminator);
      end return;
   end First_Line;

   ----------
   -- Line --
   ----------

   function Line
     (Self            : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator is
   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Lines.Line_Iterator
      do
         if not VSS.Implementation.Strings.Is_Invalid (Position) then
            Result.Initialize (Self, Position, Terminators, Keep_Terminator);
         end if;
      end return;
   end Line;

end VSS.Strings.Cursors.Iterators.Lines.Internals;
