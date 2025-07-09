--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic implementation of the constant text which use UTF-8 encoding.

--  with VSS.Implementation.Interfaces_C;
--  with VSS.Implementation.UTF8_Encoding;
with VSS.Implementation.UTF8_Strings;

package VSS.Implementation.Text_Handlers.UTF8
  with Preelaborate
is

   type UTF8_Text is
     new VSS.Implementation.Text_Handlers.Abstract_Text_Handler with
   record
      Data : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
   end record;
   --  This type is a wrapper around UTF8_String_Data.

   --  overriding function Is_Equal
   --    (Self  : Abstract_UTF8_Text;
   --     Other : Abstract_Text_Handler'Class) return Boolean;
   --  overriding function Is_Less
   --    (Self  : Abstract_UTF8_Text;
   --     Other : Abstract_Text_Handler'Class) return Boolean;
   --  overriding function Is_Less_Or_Equal
   --    (Self  : Abstract_UTF8_Text;
   --     Other : Abstract_Text_Handler'Class) return Boolean;

   procedure Unsafe_Initialize
     (Text : in out
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class);
   --  Initialize UTF-8 text storage to "null" state.
   --
   --  Memory for Text object is used to initialize new object, thus any
   --  content is destroyed, and "type" of the object will change.

   procedure Unsafe_Initialize
     (Text : in out
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class;
      Size : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Initialize UTF-8 text storage. It is either static or dynamic,
   --  depending from the requested capacity and/or size, which is larger.
   --
   --  Memory for Text object is used to initialize new object, thus any
   --  content is destroyed, and "type" of the object will change.

private

   overriding procedure After_Last_Character
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding procedure Append
     (Self   : in out UTF8_Text;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding function Backward
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Before_First_Character
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding procedure Delete
     (Self : in out UTF8_Text;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   overriding function Element
     (Self     : UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base;

   overriding function Forward
     (Self     : UTF8_Text;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean;

   overriding procedure From_Wide_Wide_String
     (Self    : in out UTF8_Text;
      Item    : Wide_Wide_String;
      Success : out Boolean);

   --  not overriding procedure From_UTF_8_String
   --    (Self    : in out Abstract_Text_Handler;
   --     Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
   --     Success : out Boolean);
   --  --  Convert UTF_8_String into internal representation. Default
   --  --  implementation decode text character-by-character and append decoded
   --  --  characters to the result.

   overriding function Has_Character
     (Self     : UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Insert
     (Self   : in out UTF8_Text;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding function Is_Empty (Self : UTF8_Text) return Boolean;

   overriding function Length
     (Self : UTF8_Text) return VSS.Implementation.Strings.Character_Count;

   overriding procedure Reference (Self : in out UTF8_Text);

   overriding procedure Split_Lines
     (Self            : UTF8_Text;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   overriding function To_UTF_8_String
     (Self : UTF8_Text) return Ada.Strings.UTF_Encoding.UTF_8_String;

   overriding procedure Unreference (Self : in out UTF8_Text);

   --  not overriding function Is_Null
   --    (Self : Abstract_Text_Handler) return Boolean;
   --  --  Return True when string is null.
   --  --
   --  --  Default implementation returns False always. Null_Handler overrides
   --  --  this oprtation to return True, other handlers can use default
   --  --  implementation.
   --
   --  not overriding procedure Hash
   --    (Self      : Abstract_Text_Handler;
   --     Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator);
   --  --  Compute hash value of the string as little-endian UTF-32 encoded
   --  --  character sequence.

   --  not overriding function Forward_Element
   --    (Self     : Abstract_Text_Handler;
   --     Position : aliased in out VSS.Implementation.Strings.Cursor;
   --     Element  : out VSS.Unicode.Code_Point'Base) return Boolean;
   --  --  Move cursor one character forward. Return True on success. Sets Element
   --  --  to the character at new position, or to No_Character.
   --
   --  not overriding function Is_Equal
   --    (Self  : Abstract_Text_Handler;
   --     Other : Abstract_Text_Handler'Class) return Boolean;
   --  not overriding function Is_Less
   --    (Self  : Abstract_Text_Handler;
   --     Other : Abstract_Text_Handler'Class) return Boolean;
   --  not overriding function Is_Less_Or_Equal
   --    (Self  : Abstract_Text_Handler;
   --     Other : Abstract_Text_Handler'Class) return Boolean;
   --  --  Compare two strings for binary equivalence/less/greater of code point
   --  --  sequences. These subprograms provides generic implementation and can
   --  --  work with any string handlers in cost of performance. Derived types may
   --  --  provide better implementation for particular case, but always should
   --  --  fallback to this implementation.
   --
   --  pragma Warnings (Off, "redundant conversion, ""SELF"" is of type");
   --  --  Disable warning for conversion to class-wide type in preconditions,
   --  --  call to Length must be dispatched to actual implementation.
   --
   --  not overriding function Starts_With
   --    (Self   : Abstract_Text_Handler;
   --     Prefix : Abstract_Text_Handler'Class) return Boolean
   --    with Pre'Class =>
   --      Abstract_Text_Handler'Class (Self).Length >= Prefix.Length;
   --  --  Return True when string starts with given prefix. This subprogram
   --  --  provides generic implementation and can work with any string handlers
   --  --  in cost of performance.
   --  not overriding function Ends_With
   --    (Self   : Abstract_Text_Handler;
   --     Suffix : Abstract_Text_Handler'Class) return Boolean
   --    with Pre'Class =>
   --      Abstract_Text_Handler'Class (Self).Length >= Suffix.Length;
   --  --  Return True when string ends with given suffix. This subprogram
   --  --  provides generic implementation and can work with any string handlers
   --  --  in cost of performance.
   --
   --  pragma Warnings (On, "redundant conversion, ""SELF"" is of type");
   --
   --  not overriding function First_UTF8_Offset
   --    (Self     : Abstract_Text_Handler;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --  --  Return UTF-8 offset at the given position, when corresponding member
   --  --  of the cursor has negative value.
   --
   --  not overriding function Last_UTF8_Offset
   --    (Self     : Abstract_Text_Handler;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --  --  Return offset of the last UTF-8 code unit at the given position.
   --
   --  not overriding function First_UTF16_Offset
   --    (Self     : Abstract_Text_Handler;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --  --  Return UTF-16 offset at the given position, when corresponding member
   --  --  of the cursor has negative value.
   --
   --  not overriding function Last_UTF16_Offset
   --    (Self     : Abstract_Text_Handler;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --  --  Return offset of the last UTF-16 code unit at the given position.
   --
   --  not overriding procedure Compute_Size
   --    (Self   : Abstract_Text_Handler;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     To     : VSS.Implementation.Strings.Cursor;
   --     Size   : out VSS.Implementation.Strings.Cursor_Offset);
   --  --  Compute size of the given segment. All components of Size have valid
   --  --  and positive values.
   --
   --  not overriding procedure Append
   --    (Self   : in out Abstract_Text_Handler;
   --     Data   : in out VSS.Implementation.Strings.String_Data;
   --     Suffix : VSS.Implementation.Strings.String_Data;
   --     Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  --  Append suffix string to the data.
   --  --  The default implementation append string in a character by character
   --  --  way.
   --  --
   --  --  Implementation must increment value of the Offset.
   --
   --  not overriding procedure Insert
   --    (Self   : in out Abstract_Text_Handler;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     Item   : VSS.Implementation.Strings.String_Data;
   --     Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  --  Insert string into the string.
   --  --
   --  --  Implementation must increment value of the Offset.
   --
   --  not overriding procedure Delete
   --    (Self : in out Abstract_Text_Handler;
   --     From : VSS.Implementation.Strings.Cursor;
   --     Size : VSS.Implementation.Strings.Cursor_Offset) is abstract;
   --  --  Delete segment of given size starting from given position.
   --
   --  not overriding procedure Slice
   --    (Self   : Abstract_Text_Handler;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     To     : VSS.Implementation.Strings.Cursor;
   --     Target : out VSS.Implementation.Strings.String_Data);
   --  --  Return slice of the string.
   --  --  Default implementation construct slice by processing individual
   --  --  characters.
   --
   --  not overriding procedure Split
   --    (Self             : Abstract_Text_Handler;
   --     Data             : VSS.Implementation.Strings.String_Data;
   --     Separator        : VSS.Unicode.Code_Point;
   --     Keep_Empty_Parts : Boolean;
   --     Items            : in out
   --       VSS.Implementation.String_Vectors.String_Vector_Data_Access);
   --  --  Split the string into substrings where separator occurs and return
   --  --  list of those strings.

end VSS.Implementation.Text_Handlers.UTF8;
