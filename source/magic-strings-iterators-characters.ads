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

package Magic.Strings.Iterators.Characters is

   pragma Preelaborate;

   type Character_Iterator is new Abstract_Iterator with private;

   function Element
     (Self : Character_Iterator'Class) return Magic.Characters.Magic_Character;
   --  Return character pointed by iterator.

private

   type Character_Iterator is new Abstract_Iterator with null record;

   overriding function Forward
     (Self : in out Character_Iterator) return Boolean;

end Magic.Strings.Iterators.Characters;
