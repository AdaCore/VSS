------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body Magic.Strings.Iterators.Characters.Internals is

   ---------------------
   -- First_Character --
   ---------------------

   function First_Character
     (Self : Magic_String'Class)
      return Magic.Strings.Iterators.Characters.Character_Iterator is
   begin
      return Result : Magic.Strings.Iterators.Characters.Character_Iterator do
         Result.Connect (Self'Unrestricted_Access);

         if Self.Data /= null then
            Self.Data.First_Character (Result.Position);
         end if;
      end return;
   end First_Character;

end Magic.Strings.Iterators.Characters.Internals;
