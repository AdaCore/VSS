--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Internal representation of UTF-8 encoded text.
--
--  Note, UTF8 encoded text must be valid (shortest encoding form, no
--  surrogates), otherwise optimized operations might return incorrect result.

pragma Ada_2022;

with Ada.Strings.UTF_Encoding;
with Interfaces;
with System.Storage_Elements;

with VSS.Implementation.FNV_Hash;
with VSS.Implementation.Strings;
limited with VSS.Implementation.String_Vectors;
limited with VSS.Strings;
with VSS.Unicode;

package VSS.Implementation.UTF8_Strings
  with Preelaborate
is

   type UTF8_String_Data is record
      Manager         : System.Storage_Elements.Storage_Array (0 .. 15) :=
        [others => 0];
      Flags           : Interfaces.Unsigned_32                          := 0;
      Size            : VSS.Unicode.UTF8_Code_Unit_Count                := 0;
      --  Number of code units in the buffer.
      Storage_Address : System.Address                                  :=
        System.Null_Address;
      Length          : VSS.Implementation.Strings.Character_Count      := 0;
      --  Length of the string in Unicode Code Points.
   end record with Preelaborable_Initialization;
      --  Alignment   => 8,
      --      Object_Size => 256;

   procedure Reference (Self : in out UTF8_String_Data);
   --  Increment reference counter of the storage of given text.

   procedure Unreference (Self : in out UTF8_String_Data);
   --  Decrement reference counter of the storage of given text.

   procedure Adjust (Self : in out UTF8_String_Data);
   --  Adjust text at the new location. Reference counter of the storage is not
   --  changed.

   function Is_Empty (Self : UTF8_String_Data) return Boolean with Inline;
   --  Return True when string is empty (null string is an empty string).

   function Is_Null (Self : UTF8_String_Data) return Boolean with Inline;
   --  Return True when string is null.

   function Is_Equal
     (Left : UTF8_String_Data; Right : UTF8_String_Data) return Boolean;
   --  Compare two strings for binary equivalence of code point sequences.
   --  Returns `True` when strings are equal.

   function Is_Less
     (Left : UTF8_String_Data; Right : UTF8_String_Data) return Boolean;
   --  Compare two strings for binary equivalence of code point sequences.
   --  Returns `True` when `Left` string is less than `Right`

   function Is_Less_Or_Equal
     (Left : UTF8_String_Data; Right : UTF8_String_Data) return Boolean;
   --  Compare two strings for binary equivalence of code point sequences.
   --  Returns `True` when `Left` string is less than `Right`, or strings
   --  are equal.

   function Has_Character
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Returns `True` when position points to a character.

   procedure Hash
     (Text      : UTF8_String_Data;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator);
   --  Compute hash value of the string as little-endian UTF-32 encoded
   --  character sequence.

   function Starts_With
     (Text : UTF8_String_Data; Prefix : UTF8_String_Data) return Boolean;
   --  Return True when string starts with given prefix.

   function Starts_With
     (Text : UTF8_String_Data; Prefix : VSS.Unicode.Code_Point) return Boolean;
   --  Return True when string starts with given prefix.

   function Ends_With
     (Text : UTF8_String_Data; Suffix : UTF8_String_Data) return Boolean;
   --  Return True when string ends with given suffix.

   function Ends_With
     (Text   : UTF8_String_Data;
      Suffix : VSS.Unicode.Code_Point) return Boolean;
   --  Return True when string ends with given suffix.

   procedure Unchecked_Forward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
        with Inline_Always;
   --  Move cursor to position of the next character.

   procedure Unchecked_Backward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Move cursor to position of the previous character

   procedure Unchecked_Backward_Decode
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point);
   --  Change offset to the point of the previous character and decode
   --  character at this position.

   procedure Unchecked_Decode_Forward
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point);
   --  Decode UTF8 encoded character started at given offset and change offset
   --  to point to the beginning of the next character.

   function Element
     (Self     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base;

   function To_UTF_8_String
     (Self : UTF8_String_Data) return Ada.Strings.UTF_Encoding.UTF_8_String;

   procedure Slice
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Result : out VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Return slice of the string.

   procedure Split
     (Text             : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Separator        : VSS.Unicode.Code_Point;
      Keep_Empty_Parts : Boolean;
      Items            : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);
   --  Split the string into substrings where separator occurs and return
   --  list of those strings.

   procedure Split_Lines
     (Text            : UTF8_String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   procedure Before_First_Character
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Initialize iterator to point before the first character.

   procedure At_First_Character
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Initialize iterator to point at the first character.

   procedure After_Last_Character
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Initialize iterator to point after the last character.

   procedure Compute_Size
     (Text   : UTF8_String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Size   : out VSS.Implementation.Strings.Cursor_Offset);
   --  Compute size of the given segment. All components of Size have valid
   --  and positive values.

   function Forward
     (Text     : UTF8_String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean;
   --  Move cursor one character forward. Return True on success.

   function Forward_Element
     (Text     : UTF8_String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean;
   --  Move cursor one character forward. Return True on success. Sets Element
   --  to the character at new position, or to No_Character.

   function Backward
     (Text     : UTF8_String_Data;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean;
   --  Move cursor one character backward. Return True on success.

   overriding function "="
     (Left  : UTF8_String_Data;
      Right : UTF8_String_Data) return Boolean is abstract;

   function Last_UTF8_Offset
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Return offset of the last UTF-8 code unit at the given position.

   function First_UTF16_Offset
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return UTF-16 offset at the given position, when corresponding member
   --  of the cursor has negative value.

   function Last_UTF16_Offset
     (Text     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Return offset of the last UTF-16 code unit at the given position.

private

   Default_UTF8_String_Data : constant UTF8_String_Data := (others => <>);

   SSO_Max_Size : constant := 15;
   --  XXX 64bit only !!!

   function Is_SSO (Self : UTF8_String_Data) return Boolean;

end VSS.Implementation.UTF8_Strings;
