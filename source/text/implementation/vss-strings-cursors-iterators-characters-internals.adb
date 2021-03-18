------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

package body VSS.Strings.Cursors.Iterators.Characters.Internals is

   use type VSS.Implementation.Strings.String_Handler_Access;

   ---------------
   -- Character --
   ---------------

   function Character
     (Self     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      if Position in Character_Cursor_Limited_Base'Class then
         declare
            P : Character_Cursor_Limited_Base'Class
              renames Character_Cursor_Limited_Base'Class (Position);

         begin
            if P.Owner /= Self'Unrestricted_Access then
               raise Program_Error;
            end if;

            return Result :
              VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
            do
               Result.Connect (Self'Unrestricted_Access);
               Result.Position := P.Position;
            end return;
         end;

      elsif Position in Character_Cursor_Base'Class then
         declare
            P : Character_Cursor_Base'Class
              renames Character_Cursor_Base'Class (Position);

         begin
            if P.Owner /= Self'Unrestricted_Access then
               raise Program_Error;
            end if;

            return Result :
              VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
            do
               Result.Connect (Self'Unrestricted_Access);
               Result.Position := P.Position;
            end return;
         end;

      else
         --  Aux : constant VSS.Implementation.Strings.Cursor :=
         --    (Position.Character_Index,
         --     Position.First_UTF8_Offset,
         --     Position.Last_UTF8_Offset);

         raise Program_Error;
      end if;
   end Character;

   ---------------------
   -- First_Character --
   ---------------------

   function First_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
   is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        Self.Handler;
      Dummy   : Boolean;

   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
      do
         Result.Connect (Self'Unrestricted_Access);

         if Handler /= null then
            Handler.Before_First_Character (Self.Data, Result.Position);
            Dummy := Handler.Forward (Self.Data, Result.Position);
         end if;
      end return;
   end First_Character;

end VSS.Strings.Cursors.Iterators.Characters.Internals;
