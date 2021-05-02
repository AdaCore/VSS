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

   type Abstract_Character_Iterator is
     abstract limited new VSS.Strings.Cursors.Abstract_Character_Cursor
       with private;

   function Forward
     (Self : in out Abstract_Character_Iterator) return Boolean is abstract;
   --  Move cursor one character forward

   function Backward
     (Self : in out Abstract_Character_Iterator) return Boolean is abstract;
   --  Move cursor one character backward

   function Has_Element
     (Self : Abstract_Character_Iterator) return Boolean is abstract;
   --  Returns True when iterator points to the text element

   type Abstract_Segment_Iterator is
     abstract limited new VSS.Strings.Cursors.Abstract_Segment_Cursor
       with private;

   function Forward
     (Self : in out Abstract_Segment_Iterator) return Boolean is abstract;
   --  Move cursor to the next segment

   function Has_Element
     (Self : Abstract_Segment_Iterator) return Boolean is abstract;
   --  Return True when iterator points to the text element

private

   type Abstract_Character_Iterator is
     abstract limited new VSS.Strings.Cursors.Character_Cursor_Limited_Base
       with null record;

   overriding procedure String_Modified
     (Self     : in out Abstract_Character_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is abstract;

   type Abstract_Segment_Iterator is
     abstract limited new VSS.Strings.Cursors.Segment_Cursor_Limited_Base
       with null record;

   overriding procedure String_Modified
     (Self     : in out Abstract_Segment_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is abstract;

end VSS.Strings.Cursors.Iterators;
