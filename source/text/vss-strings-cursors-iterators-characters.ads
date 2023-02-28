--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Characters;
private with VSS.Strings.Cursors.Markers;
pragma Unreferenced (VSS.Strings.Cursors.Markers);
--  XXX GNAT 20210308 reports error without with clause above.

package VSS.Strings.Cursors.Iterators.Characters is

   pragma Preelaborate;

   type Character_Iterator is new Abstract_Character_Iterator with private;

   function Element
     (Self : Character_Iterator'Class) return VSS.Characters.Virtual_Character;
   --  Return character pointed by iterator.

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
