--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Abstract_String_Handler is abstract set of operations on string data.
--  It provides default generic implementation of some operations which
--  derived handlers may override to provide better implementation.
--
--  Note, when adding new operation it must be overridden by the
--  Null_String_Handler with proper preconditions and implementation.

with Ada.Strings.UTF_Encoding;

with VSS.Implementation.FNV_Hash;
with VSS.Implementation.Strings;
with VSS.Implementation.String_Vectors;
limited with VSS.Strings;
with VSS.Unicode;

package VSS.Implementation.Text_Handlers is

   pragma Preelaborate;

   use type VSS.Implementation.Strings.Character_Count;

   -----------------------------
   -- Abstract_String_Handler --
   -----------------------------

   type Abstract_String_Handler is abstract tagged null record;

   not overriding procedure Reference
     (Self : in out Abstract_String_Handler) is abstract;
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   not overriding procedure Unreference
     (Self : in out Abstract_String_Handler) is abstract;
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

   not overriding function Is_Empty
     (Self : Abstract_String_Handler) return Boolean is abstract;
   --  Return True when string is empty.

   not overriding function Is_Null
     (Self : Abstract_String_Handler) return Boolean;
   --  Return True when string is null.

   not overriding procedure Hash
     (Self      : Abstract_String_Handler;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator);
   --  Compute hash value of the string as little-endian UTF-32 encoded
   --  character sequence.

   not overriding function Length
     (Self : Abstract_String_Handler)
      return VSS.Implementation.Strings.Character_Count is abstract;
   --  Return number of characters in the text

   not overriding function Element
     (Self     : Abstract_String_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base is abstract;
   --  Return character at given position or NUL if Position is not pointing
   --  to any character.

   not overriding function Has_Character
     (Self     : Abstract_String_Handler;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is abstract;
   --  Return True when position points to the character.

   not overriding procedure Before_First_Character
     (Self     : Abstract_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is abstract;

   not overriding procedure After_Last_Character
     (Self     : Abstract_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is abstract;
   --  Initialize iterator to point to the last character.
   --  This procedure sets Position.UTF16_Offset to UTF16_Code_Unit_Index'Last
   --  for now.

   not overriding function Forward
     (Self     : Abstract_String_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean is abstract;
   --  Move cursor one character forward. Return True on success.

   not overriding function Backward
     (Self     : Abstract_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor)
      return Boolean is abstract;
   --  Move cursor one character backward. Return True on success.

   not overriding function Forward_Element
     (Self     : Abstract_String_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean;
   --  Move cursor one character forward. Return True on success. Sets Element
   --  to the character at new position, or to No_Character.

   not overriding function Is_Equal
     (Self  : Abstract_String_Handler;
      Other : Abstract_String_Handler'Class) return Boolean;
   not overriding function Is_Less
     (Self  : Abstract_String_Handler;
      Other : Abstract_String_Handler'Class) return Boolean;
   not overriding function Is_Less_Or_Equal
     (Self  : Abstract_String_Handler;
      Other : Abstract_String_Handler'Class) return Boolean;
   --  Compare two strings for binary equivalence/less/greater of code point
   --  sequences. These subprograms provides generic implementation and can
   --  work with any string handlers in cost of performance. Derived types may
   --  provide better implementation for particular case, but always should
   --  fallback to this implementation.

   pragma Warnings (Off, "redundant conversion, ""SELF"" is of type");
   --  Disable warning for conversion to class-wide type in preconditions,
   --  call to Length must be dispatched to actual implementation.

   not overriding function Starts_With
     (Self   : Abstract_String_Handler;
      Prefix : Abstract_String_Handler'Class) return Boolean
     with Pre'Class =>
       Abstract_String_Handler'Class (Self).Length >= Prefix.Length;
   --  Return True when string starts with given prefix. This subprogram
   --  provides generic implementation and can work with any string handlers
   --  in cost of performance.
   not overriding function Ends_With
     (Self   : Abstract_String_Handler;
      Suffix : Abstract_String_Handler'Class) return Boolean
     with Pre'Class =>
       Abstract_String_Handler'Class (Self).Length >= Suffix.Length;
   --  Return True when string ends with given suffix. This subprogram
   --  provides generic implementation and can work with any string handlers
   --  in cost of performance.

   pragma Warnings (On, "redundant conversion, ""SELF"" is of type");

   not overriding procedure From_Wide_Wide_String
     (Self    : in out Abstract_String_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean) is abstract;

   not overriding procedure From_UTF_8_String
     (Self    : in out Abstract_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean);
   --  Convert UTF_8_String into internal representation. Default
   --  implementation decode text character-by-character and append decoded
   --  characters to the result.

   not overriding function To_UTF_8_String
     (Self : Abstract_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Ada.Strings.UTF_Encoding.UTF_8_String is abstract;
   --  Converts string data into standard UTF_8_String.

   not overriding function First_UTF8_Offset
     (Self     : Abstract_String_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return UTF-8 offset at the given position, when corresponding member
   --  of the cursor has negative value.

   not overriding function Last_UTF8_Offset
     (Self     : Abstract_String_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the last UTF-8 code unit at the given position.

   not overriding function First_UTF16_Offset
     (Self     : Abstract_String_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return UTF-16 offset at the given position, when corresponding member
   --  of the cursor has negative value.

   not overriding function Last_UTF16_Offset
     (Self     : Abstract_String_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the last UTF-16 code unit at the given position.

   not overriding procedure Compute_Size
     (Self   : Abstract_String_Handler;
      Data   : VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Size   : out VSS.Implementation.Strings.Cursor_Offset);
   --  Compute size of the given segment. All components of Size have valid
   --  and positive values.

   not overriding procedure Append
     (Self   : in out Abstract_String_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is abstract
     with Pre'Class => Code not in 16#D800# .. 16#DFFF#;
   --  Append single character to the text data.
   --
   --  Implementation must increment value of the Offset.

   not overriding procedure Append
     (Self   : in out Abstract_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Append suffix string to the data.
   --  The default implementation append string in a character by character
   --  way.
   --
   --  Implementation must increment value of the Offset.

   not overriding procedure Insert
     (Self   : in out Abstract_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is abstract;
   --  Insert single code point into the string.
   --
   --  Implementation must increment value of the Offset.

   not overriding procedure Insert
     (Self   : in out Abstract_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Insert string into the string.
   --
   --  Implementation must increment value of the Offset.

   not overriding procedure Delete
     (Self : in out Abstract_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset) is abstract;
   --  Delete segment of given size starting from given position.

   not overriding procedure Slice
     (Self   : Abstract_String_Handler;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Target : out VSS.Implementation.Strings.String_Data);
   --  Return slice of the string.
   --  Default implementation construct slice by processing individual
   --  characters.

   not overriding procedure Split
     (Self             : Abstract_String_Handler;
      Data             : VSS.Implementation.Strings.String_Data;
      Separator        : VSS.Unicode.Code_Point;
      Keep_Empty_Parts : Boolean;
      Case_Sensitivity : VSS.Strings.Case_Sensitivity;
      Items            : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);
   --  Split the string into substrings where separator occurs and return
   --  list of those strings.

   not overriding procedure Split_Lines
     (Self            : Abstract_String_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
      is abstract;
   --  Splits string into lines using given set of allowed new line
   --  terminators. Line terminator (character or combination of characters)
   --  are removed unless Keep_Terminator is set to True.

   function Unsafe_Capacity
     (Self : Abstract_String_Handler'Class)
      return VSS.Implementation.Strings.Character_Count;
   --  Returns capacity of the text data, requested by the application for the
   --  given text daya.
   --
   --  Implementation assume that given text data is inside of the String_Data
   --  object.

end VSS.Implementation.Text_Handlers;
