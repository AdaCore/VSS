--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   --  ??? Move functions below to Abstract_Iterator interface?

   --  function Before_First
   --    (Item : VSS.Strings.Virtual_String'Class)
   --     return Abstract_Segment_Iterator is abstract;
   --  --  Create iterator pointing before the first segment of the given
   --  --  string.
   --
   --  function At_First
   --    (Item : VSS.Strings.Virtual_String'Class)
   --     return Abstract_Segment_Iterator is abstract;
   --  --  Create iterator pointing to the first segment of the given string.
   --
   --  --  function At_Position
   --  --    (Item     : VSS.Strings.Virtual_String'Class;
   --
   --  function At_Last
   --    (Item : VSS.Strings.Virtual_String'Class)
   --     return Abstract_Segment_Iterator is abstract;
   --  --  Create iterator pointing to the last segment of the given string.
   --
   --  function After_Last
   --    (Item : VSS.Strings.Virtual_String'Class)
   --     return Abstract_Segment_Iterator is abstract;
   --  --  Create iterator pointing after the last segment of the given string.
   --
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
