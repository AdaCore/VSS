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

with Magic.Unicode;

package Magic.Strings.Iterators is

   pragma Preelaborate;

   type Abstract_Iterator is abstract tagged limited private;

   function Character_Index
     (Self : Abstract_Iterator'Class) return Magic.Strings.Character_Index;

   function UTF8_Offset
     (Self : Abstract_Iterator'Class)
      return Magic.Unicode.UTF8_Code_Unit_Index;

   function UTF16_Offset
     (Self : Abstract_Iterator'Class)
      return Magic.Unicode.UTF16_Code_Unit_Index;

   function Forward
     (Self : in out Abstract_Iterator) return Boolean is abstract;

private

   type Abstract_Iterator is abstract new Referal_Limited_Base with record
      Position : Cursor;
   end record;

   overriding procedure Invalidate (Self : in out Abstract_Iterator);

end Magic.Strings.Iterators;
