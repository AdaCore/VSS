------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
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

with VSS.Characters;
private with VSS.Strings.Cursors.Markers;
pragma Unreferenced (VSS.Strings.Cursors.Markers);
--  XXX GNAT 20210308 reports error whithout with clause above.

package VSS.Strings.Cursors.Iterators.Characters is

   pragma Preelaborate;

   type Character_Iterator is new Abstract_Character_Iterator with private;

   function Element
     (Self : Character_Iterator'Class) return VSS.Characters.Virtual_Character;
   --  Return character pointed by iterator.

   procedure Set_At_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point to the first text element of the given
   --  string.

   procedure Set_Before_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point before the first text element of the given
   --  string.

   procedure Set_At_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point to the last text element of the given
   --  string.

   procedure Set_After_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point after the last text element of the given
   --  string.

   procedure Set_At
     (Self     : in out Character_Iterator;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class);
   --  Initialize iterator to point at the text element at given position.

private

   type Character_Iterator is new Abstract_Character_Iterator with null record;

   overriding procedure String_Modified
     (Self     : in out Character_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Deleted  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

   overriding function Backward
     (Self : in out Character_Iterator) return Boolean;

   overriding function Forward
     (Self : in out Character_Iterator) return Boolean;

   overriding function Has_Element (Self : Character_Iterator) return Boolean;

end VSS.Strings.Cursors.Iterators.Characters;
