--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

limited with VSS.Strings.Cursors.Markers;
with VSS.Unicode;

package VSS.Strings.Cursors is

   pragma Preelaborate;

   ---------------------
   -- Abstract_Cursor --
   ---------------------

   type Abstract_Cursor is limited interface;

   function Is_Valid (Self : Abstract_Cursor) return Boolean is abstract;
   --  Return True when cursor is valid: it points to some logical element
   --  in the string, including logical element before the string and after
   --  the string when applicable.

   function First_Marker
     (Self : Abstract_Cursor)
      return VSS.Strings.Cursors.Markers.Character_Marker is abstract;
   --  Return marker of the first character of the logical element.

   function Last_Marker
     (Self : Abstract_Cursor)
      return VSS.Strings.Cursors.Markers.Character_Marker is abstract;
   --  Return marker of the last character of the logical element.

   function First_Character_Index
     (Self : Abstract_Cursor)
      return VSS.Strings.Character_Index'Base is abstract;
   --  Return index of the first character of the logical element. This index
   --  is zero when cursor is invalid or points before the first character of
   --  the string data; or large than length of the string when cursor points
   --  after last characters of the string data.

   function Last_Character_Index
     (Self : Abstract_Cursor)
      return VSS.Strings.Character_Index'Base is abstract;
   --  Return index of the last character of the logical element. Returned
   --  value when cursor points to the empty logical element or to the logical
   --  element before or after the string data depends from the particular
   --  implementation.

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

   -------------------------------
   -- Abstract_Character_Cursor --
   -------------------------------

   type Abstract_Character_Cursor is limited interface and Abstract_Cursor;
   --  Cursor that points to single character.

   function Marker
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Return marker of the character.

   function Character_Index
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Strings.Character_Index'Base;
   --  Returns index of the character.

   -----------------------------
   -- Abstract_Segment_Cursor --
   -----------------------------

   type Abstract_Segment_Cursor is limited interface and Abstract_Cursor;
   --  Cursor that points to some segment of the string.

   function Character_Length
     (Self : Abstract_Segment_Cursor)
      return VSS.Strings.Character_Count is abstract;
   --  Length of the segment in characters.

private

   ---------------------------
   -- Character_Cursor_Base --
   ---------------------------

   type Character_Cursor_Base is
     abstract new VSS.Implementation.Referrers.Referal_Base
       and VSS.Strings.Cursors.Abstract_Character_Cursor with
   record
      Position : aliased VSS.Implementation.Strings.Cursor;
   end record;

   overriding procedure Invalidate (Self : in out Character_Cursor_Base);

   overriding function Is_Valid (Self : Character_Cursor_Base) return Boolean;

   overriding function First_Marker
     (Self : Character_Cursor_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker;

   overriding function Last_Marker
     (Self : Character_Cursor_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker renames First_Marker;

   overriding function First_Character_Index
     (Self : Character_Cursor_Base)
      return VSS.Strings.Character_Index'Base;

   overriding function Last_Character_Index
     (Self : Character_Cursor_Base)
      return VSS.Strings.Character_Index'Base renames First_Character_Index;

   overriding function First_UTF8_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function Last_UTF8_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function First_UTF16_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Last_UTF16_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index;

   -----------------------------------
   -- Character_Cursor_Limited_Base --
   -----------------------------------

   type Character_Cursor_Limited_Base is
     abstract new VSS.Implementation.Referrers.Referal_Limited_Base
       and VSS.Strings.Cursors.Abstract_Character_Cursor with
   record
      Position : aliased VSS.Implementation.Strings.Cursor;
   end record;

   overriding procedure Invalidate
     (Self : in out Character_Cursor_Limited_Base);

   overriding function Is_Valid
     (Self : Character_Cursor_Limited_Base) return Boolean;

   overriding function First_Marker
     (Self : Character_Cursor_Limited_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker;

   overriding function Last_Marker
     (Self : Character_Cursor_Limited_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker renames First_Marker;

   overriding function First_Character_Index
     (Self : Character_Cursor_Limited_Base)
      return VSS.Strings.Character_Index'Base;

   overriding function Last_Character_Index
     (Self : Character_Cursor_Limited_Base)
      return VSS.Strings.Character_Index'Base renames First_Character_Index;

   overriding function First_UTF8_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function Last_UTF8_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function First_UTF16_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Last_UTF16_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index;

   -------------------------
   -- Segment_Cursor_Base --
   -------------------------

   type Segment_Cursor_Base is
     abstract new VSS.Implementation.Referrers.Referal_Base
       and VSS.Strings.Cursors.Abstract_Segment_Cursor with
   record
      First_Position : aliased VSS.Implementation.Strings.Cursor;
      Last_Position  : aliased VSS.Implementation.Strings.Cursor;
   end record;

   overriding procedure Invalidate (Self : in out Segment_Cursor_Base);

   overriding function Is_Valid (Self : Segment_Cursor_Base) return Boolean;

   overriding function First_Marker
     (Self : Segment_Cursor_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker;

   overriding function Last_Marker
     (Self : Segment_Cursor_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker;

   overriding function First_Character_Index
     (Self : Segment_Cursor_Base)
      return VSS.Strings.Character_Index'Base;

   overriding function Last_Character_Index
     (Self : Segment_Cursor_Base) return VSS.Strings.Character_Index'Base;

   overriding function First_UTF8_Offset
     (Self : Segment_Cursor_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function Last_UTF8_Offset
     (Self : Segment_Cursor_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function First_UTF16_Offset
     (Self : Segment_Cursor_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Last_UTF16_Offset
     (Self : Segment_Cursor_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Character_Length
     (Self : Segment_Cursor_Base) return VSS.Strings.Character_Count;

   ---------------------------------
   -- Segment_Cursor_Limited_Base --
   ---------------------------------

   type Segment_Cursor_Limited_Base is
     abstract limited new VSS.Implementation.Referrers.Referal_Limited_Base
       and VSS.Strings.Cursors.Abstract_Segment_Cursor with
   record
      First_Position : aliased VSS.Implementation.Strings.Cursor;
      Last_Position  : aliased VSS.Implementation.Strings.Cursor;
   end record;

   overriding procedure Invalidate (Self : in out Segment_Cursor_Limited_Base);

   overriding function Is_Valid
     (Self : Segment_Cursor_Limited_Base) return Boolean;

   overriding function First_Marker
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker;

   overriding function Last_Marker
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker;

   overriding function First_Character_Index
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Character_Index'Base;

   overriding function Last_Character_Index
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Character_Index'Base;

   overriding function First_UTF8_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function Last_UTF8_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index;

   overriding function First_UTF16_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Last_UTF16_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Character_Length
     (Self : Segment_Cursor_Limited_Base) return VSS.Strings.Character_Count;

   ---------------
   -- Utilities --
   ---------------

   function First_UTF8_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Common code to obtain offset of the first UTF8 code unit for given
   --  position.

   function Last_UTF8_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Common code to obtain offset of the last UTF8 code unit for given
   --  position.

   function First_UTF16_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Common code to obtain offset of the first UTF16 code unit for given
   --  position.

   function Last_UTF16_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Common code to obtain offset of the last UTF16 code unit for given
   --  position.

end VSS.Strings.Cursors;
