--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package VSS.Strings.Cursors.Iterators.Lines is

   pragma Preelaborate;

   type Line_Iterator is new Abstract_Segment_Iterator with private;

   function Has_Line_Terminator (Self : Line_Iterator'Class) return Boolean;
   --  Return True when line has lime terminator sequence. Only last line
   --  of the text may not have line terminator. This function is independent
   --  from the Keep_Terminator mode.

   function Element_Terminator
     (Self : Line_Iterator'Class) return VSS.Strings.Virtual_String;
   --  Return line terminator sequence of the current line. Return empty
   --  string when line is not terminated by line termination sequence
   --  (it is last line in the source string).

   function Terminator_First_Marker
     (Self : Line_Iterator'Class)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Return marker of the first character of the line terminator sequence.

   function Terminator_Last_Marker
     (Self : Line_Iterator'Class)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Return marker of the last character of the line terminator sequence.

   function Terminator_First_Character_Index
     (Self : Line_Iterator'Class)
      return VSS.Strings.Character_Index;
   --  Return index of the first character of the line terminator sequence.

   function Terminator_Last_Character_Index
     (Self : Line_Iterator'Class)
      return VSS.Strings.Character_Index;
   --  Return index of the last character of the line terminator sequence.

   function Terminator_First_UTF8_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the first UTF-8 code unit of the line terminator
   --  sequence.

   function Terminator_Last_UTF8_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the last UTF-8 code unit of the line terminator
   --  sequence.

   function Terminator_First_UTF16_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the first UTF-16 code unit of the logical element.

   function Terminator_Last_UTF16_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the last UTF-16 code unit of the logical element.

private

   type Line_Iterator is new Abstract_Segment_Iterator with record
      Terminators         : Line_Terminator_Set;
      Keep_Terminator     : Boolean;

      Terminator_Position : VSS.Implementation.Strings.Cursor;
      --  Position of the line terminator sequence. It is position of the
      --  first character of the terminator when Keep_Terminator is True,
      --  otherwise it is position of the last character of the terminator.
   end record;

   overriding procedure Invalidate (Self : in out Line_Iterator);

   overriding procedure String_Modified
     (Self     : in out Line_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

   overriding function Forward (Self : in out Line_Iterator) return Boolean;

   overriding function Has_Element (Self : Line_Iterator) return Boolean;

   procedure Initialize
     (Self            : in out Line_Iterator'Class;
      String          : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False);
   --  Initialize iterator and lookup for line boundaries at the given
   --  position.

end VSS.Strings.Cursors.Iterators.Lines;
