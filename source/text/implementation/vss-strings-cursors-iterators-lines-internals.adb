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
      use type VSS.Implementation.Strings.String_Handler_Access;

      Handler  : constant VSS.Implementation.Strings.String_Handler_Access :=
        Self.Handler;
      Position : VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Lines.Line_Iterator
      do
         if Handler /= null then
            Handler.Before_First_Character (Self.Data, Position);
            Dummy := Handler.Forward (Self.Data, Position);

            Result.Initialize (Self, Position, Terminators, Keep_Terminator);
         end if;
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
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator
   is
      use type VSS.Implementation.Strings.String_Handler_Access;

      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        Self.Handler;

   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Lines.Line_Iterator
      do
         if Handler /= null
           and not VSS.Implementation.Strings.Is_Invalid (Position)
         then
            Result.Initialize (Self, Position, Terminators, Keep_Terminator);
         end if;
      end return;
   end Line;

end VSS.Strings.Cursors.Iterators.Lines.Internals;
