--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package VSS.Strings.Cursors.Iterators.Words is

   pragma Preelaborate;

   type Word_Iterator is new Abstract_Segment_Iterator with private;

   function Backward (Self : in out Word_Iterator) return Boolean;
   --  Move iterator to previous word.

   function At_First (Item : Virtual_String'Class) return Word_Iterator;
   --  Return iterator pointing to the first word of the string.

   function At_Last (Item : Virtual_String'Class) return Word_Iterator;
   --  Return iterator pointing to the last word of the string.

   function At_Position
     (Item     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return Word_Iterator;
   --  Return iterator pointing to the word of the string at the given
   --  position.

private

   type Word_Iterator is new Abstract_Segment_Iterator with record
      null;
   end record;

   overriding procedure Invalidate (Self : in out Word_Iterator);

   overriding procedure String_Modified
     (Self     : in out Word_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

   overriding function Forward (Self : in out Word_Iterator) return Boolean;

   overriding function Has_Element (Self : Word_Iterator) return Boolean;

   procedure Initialize
     (Self            : in out Word_Iterator'Class;
      String          : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor);
   --  Initialize iterator and lookup for word boundaries around the given
   --  position.

end VSS.Strings.Cursors.Iterators.Words;
