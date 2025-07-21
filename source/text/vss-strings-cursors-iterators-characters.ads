--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters;
private with VSS.Strings.Cursors.Markers;
pragma Unreferenced (VSS.Strings.Cursors.Markers);
--  XXX GNAT 20210308 reports error without with clause above.

package VSS.Strings.Cursors.Iterators.Characters is

   pragma Preelaborate;

   type Character_Iterator is new Abstract_Character_Iterator with private;

   function Element
     (Self : Character_Iterator'Class)
      return VSS.Characters.Virtual_Character'Base;
   --  Return character pointed by iterator. Return invalid value when iterator
   --  points outside of the text data.

   procedure Set_Before_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point before the first character of the given
   --  string.

   procedure Set_At_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point to the first character of the given
   --  string.

   procedure Set_At
     (Self     : in out Character_Iterator;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class);
   --  Initialize iterator to point at the character at given position.

   procedure Set_At_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point to the last character of the given string.

   procedure Set_After_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Initialize iterator to point after the last character of the given
   --  string.

   function Forward
     (Self    : in out Character_Iterator;
      Element : out VSS.Characters.Virtual_Character'Base) return Boolean;
   --  Moves the iterator one character forward, returning True if the new
   --  position points to any character within the string data. The element is
   --  set to the value of the new pointing character, or to an invalid
   --  character if the new position is outside of the string data.

private

   type Character_Iterator is new Abstract_Character_Iterator with record
      Data : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
   end record;

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
