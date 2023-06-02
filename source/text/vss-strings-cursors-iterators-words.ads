--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Iterator to iterate over word boundaries. Pointing element is not
--  necessary a word, it may point to the sequence of word separators too.

package VSS.Strings.Cursors.Iterators.Words is

   pragma Preelaborate;

   type Word_Iterator is new Abstract_Segment_Iterator with private;

   function Backward (Self : in out Word_Iterator) return Boolean;
   --  Move iterator to previous word.

   procedure Set_Before_First
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class);
   --  Set iterator to point before the first word of the string.

   procedure Set_At_First
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class);
   --  Set iterator to point to the first word segment of the string.

   procedure Set_At
     (Self     : in out Word_Iterator'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class);
   --  Set iterator to point to the word segment at the given position.

   procedure Set_At_Last
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class);
   --  Set iterator to point to the last word segment of the string.

   procedure Set_After_Last
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class);
   --  Set iterator to point after the last word of the string.

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

end VSS.Strings.Cursors.Iterators.Words;
