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

--  limited with VSS.Strings.Cursors.Markers;
with VSS.Unicode;

package VSS.Strings.Cursors is

   pragma Preelaborate;

   type Abstract_Cursor is limited interface;

   --  function First_Marker
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  --  Return marker of the first character of the logical element.
   --
   --  function Last_Marker
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  --  Return marker of the last character of the logical element.

   function First_Character_Index
     (Self : Abstract_Cursor)
      return VSS.Strings.Character_Index is abstract;
   --  Return index of the first character of the logical element.

   function Last_Character_Index
     (Self : Abstract_Cursor)
      return VSS.Strings.Character_Index is abstract;
   --  Return index of the last character of the logical element.

   function First_UTF8_Offset
     (Self : Abstract_Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index is abstract;
   --  Return offset of the first UTF-8 code unit of the logical element.

   function Last_UTF8_Offset
     (Self : Abstract_Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index is abstract;
   --  Return offset of the last UTF-8 code unit of the logical element.

   function First_UTF16_Offset
     (Self : Abstract_Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index is abstract;
   --  Return offset of the first UTF-16 code unit of the logical element.

   function Last_UTF16_Offset
     (Self : Abstract_Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index is abstract;
   --  Return offset of the last UTF-16 code unit of the logical element.


   type Abstract_Character_Cursor is limited interface and Abstract_Cursor;
   --  Cursor that points to single character.

   --  function Marker
   --    (Self : Abstract_Character_Cursor'Class)
   --     return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  --  Return marker of the character.

   function Character_Index
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Strings.Character_Index;
   --  Returns index of the character.

   --  function First_UTF8_Offset
   --    (Self : Abstract_Character_Cursor'Class)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --  --  Return offset of the first UTF-8 code unit of the logical element.
   --
   --  function Last_UTF8_Offset
   --    (Self : Abstract_Character_Cursor'Class)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --  --  Return offset of the last UTF-8 code unit of the logical element.
   --
   --  function First_UTF16_Offset
   --    (Self : Abstract_Character_Cursor'Class)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --  --  Return offset of the first UTF-16 code unit of the logical element.
   --
   --  function Last_UTF16_Offset
   --    (Self : Abstract_Character_Cursor'Class)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --  --  Return offset of the last UTF-16 code unit of the logical element.

   type Abstract_Segment_Cursor is limited interface and Abstract_Cursor;
   --  Cursor that points to some segment of the string.

   --  --  function First_Marker
   --  --    (Self : Abstract_Segment_Cursor'Class)
   --  --     return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  --  --  Return marker of the first character of the logical element.
   --  --
   --  --  function Last_Marker
   --  --    (Self : Abstract_Segment_Cursor'Class)
   --  --     return VSS.Strings.Cursors.Markers.Virtual_Marker;
   --  --  --  Return marker of the last character of the logical element.
   --
   --  function First_Character_Index
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Strings.Character_Index;
   --  --  Return index of the first character of the logical element.
   --
   --  function Last_Character_Index
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Strings.Character_Index;
   --  --  Return index of the last character of the logical element.
   --
   --  function First_UTF8_Offset
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --  --  Return offset of the first UTF-8 code unit of the logical element.
   --
   --  function Last_UTF8_Offset
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --  --  Return offset of the last UTF-8 code unit of the logical element.
   --
   --  function First_UTF16_Offset
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --  --  Return offset of the first UTF-16 code unit of the logical element.
   --
   --  function Last_UTF16_Offset
   --    (Self : Abstract_Segment_Cursor'Class)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --  --  Return offset of the last UTF-16 code unit of the logical element.

private

   --  type Abstract_Character_Cursor is
   --    abstract new Referal_Limited_Base with record
   --     Position : aliased VSS.Implementation.Strings.Cursor;
   --  end record;
   --
   --  overriding procedure Invalidate (Self : in out Abstract_Character_Cursor);
   --
   --  type Abstract_Segment_Cursor is
   --    abstract new Referal_Limited_Base with record
   --     First_Position : aliased VSS.Implementation.Strings.Cursor;
   --     Last_Position  : aliased VSS.Implementation.Strings.Cursor;
   --  end record;
   --
   --  overriding procedure Invalidate (Self : in out Abstract_Segment_Cursor);

end VSS.Strings.Cursors;
