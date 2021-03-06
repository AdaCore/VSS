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

   function Has_Element
     (Self : Abstract_Character_Iterator) return Boolean is abstract;
   --  Returns True when iterator points to the text element

   type Abstract_Segment_Iterator is
     abstract limited new VSS.Strings.Cursors.Abstract_Segment_Cursor
       with private;

private

   type Abstract_Character_Iterator is
     abstract limited new VSS.Strings.Referal_Limited_Base
       and VSS.Strings.Cursors.Abstract_Character_Cursor with
   record
      Position : aliased VSS.Implementation.Strings.Cursor;
   end record;

   overriding procedure Invalidate (Self : in out Abstract_Character_Iterator);

   overriding function First_Marker
     (Self : Abstract_Character_Iterator)
      return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  Return marker of the first character of the logical element.

   overriding function Last_Marker
     (Self : Abstract_Character_Iterator)
      return VSS.Strings.Cursors.Markers.Virtual_Marker renames First_Marker;
   --  Return marker of the last character of the logical element.

   overriding function First_Character_Index
     (Self : Abstract_Character_Iterator)
      return VSS.Strings.Character_Index;
   --  Return index of the first character of the logical element.

   overriding function Last_Character_Index
     (Self : Abstract_Character_Iterator)
      return VSS.Strings.Character_Index;
   --  Return index of the last character of the logical element.

   overriding function First_UTF8_Offset
     (Self : Abstract_Character_Iterator)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the first UTF-8 code unit of the logical element.

   overriding function Last_UTF8_Offset
     (Self : Abstract_Character_Iterator)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the last UTF-8 code unit of the logical element.

   overriding function First_UTF16_Offset
     (Self : Abstract_Character_Iterator)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the first UTF-16 code unit of the logical element.

   overriding function Last_UTF16_Offset
     (Self : Abstract_Character_Iterator)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the last UTF-16 code unit of the logical element.

   type Abstract_Segment_Iterator is
     abstract limited new VSS.Strings.Referal_Limited_Base
       and VSS.Strings.Cursors.Abstract_Segment_Cursor with
   record
      First_Position : aliased VSS.Implementation.Strings.Cursor;
      Last_Position  : aliased VSS.Implementation.Strings.Cursor;
   end record;

   overriding procedure Invalidate (Self : in out Abstract_Segment_Iterator);

   overriding function First_Marker
     (Self : Abstract_Segment_Iterator)
      return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  Return marker of the first character of the logical element.

   overriding function Last_Marker
     (Self : Abstract_Segment_Iterator)
      return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  Return marker of the last character of the logical element.

   overriding function First_Character_Index
     (Self : Abstract_Segment_Iterator)
      return VSS.Strings.Character_Index;
   --  Return index of the first character of the logical element.

   overriding function Last_Character_Index
     (Self : Abstract_Segment_Iterator)
      return VSS.Strings.Character_Index;
   --  Return index of the last character of the logical element.

   overriding function First_UTF8_Offset
     (Self : Abstract_Segment_Iterator)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the first UTF-8 code unit of the logical element.

   overriding function Last_UTF8_Offset
     (Self : Abstract_Segment_Iterator)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the last UTF-8 code unit of the logical element.

   overriding function First_UTF16_Offset
     (Self : Abstract_Segment_Iterator)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the first UTF-16 code unit of the logical element.

   overriding function Last_UTF16_Offset
     (Self : Abstract_Segment_Iterator)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the last UTF-16 code unit of the logical element.

end VSS.Strings.Cursors.Iterators;
