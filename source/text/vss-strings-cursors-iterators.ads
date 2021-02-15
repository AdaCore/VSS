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

package VSS.Strings.Cursors.Iterators is

   pragma Preelaborate;

   type Abstract_Iterator is
     abstract new VSS.Strings.Cursors.Abstract_Cursor with private;

   function Forward
     (Self : in out Abstract_Iterator) return Boolean is abstract;

   function Has_Element (Self : Abstract_Iterator) return Boolean is abstract;
   --  Returns True when iterator points to the text element

   function Create
     (Position : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return Abstract_Iterator is abstract;
   --  Creates iterator at given position. When given Position is not valid
   --  position of iterator's element constructed iterator has another position
   --  which points to start of logical element pointed by given position. It
   --  is allowed to return invalidated iterator in such cases too.

private

   type Abstract_Iterator is
     abstract new VSS.Strings.Cursors.Abstract_Cursor with null record;

end VSS.Strings.Cursors.Iterators;
