--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generic implementation of the string which use UTF-8 encoding for data.

with Ada.Strings.UTF_Encoding;
with System.Atomic_Counters;

with VSS.Implementation.String_Handlers;
with VSS.Implementation.String_Vectors;
with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Encoding;
with VSS.Strings;
with VSS.Unicode;

package VSS.Implementation.UTF8_String_Handlers is

   pragma Preelaborate;

   type UTF8_String_Data (Bulk : VSS.Unicode.UTF8_Code_Unit_Count) is record
      Counter : System.Atomic_Counters.Atomic_Counter;

      Storage :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array (0 .. Bulk);
      --  Buffer to store string's data. First unused code unit is set to
      --  zero, to allow to pass data to C.

      Size    : VSS.Unicode.UTF8_Code_Unit_Count           := 0;
      --  Number of code units in the buffer.

      Length  : VSS.Implementation.Strings.Character_Count := 0;
      --  Length of the string in Unicode Code Points.
   end record;

   type UTF8_String_Data_Access is access all UTF8_String_Data;

   type UTF8_String_Handler is
     new VSS.Implementation.String_Handlers.Abstract_String_Handler
       with null record;

   overriding procedure Reference
     (Self : UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data);
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   overriding procedure Unreference
     (Self : UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data);
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

   overriding procedure Initialize
     (Self : UTF8_String_Handler;
      Data : out VSS.Implementation.Strings.String_Data);

   overriding function Is_Empty
     (Self : UTF8_String_Handler;
      Data : VSS.Implementation.Strings.String_Data) return Boolean;

   overriding function Length
     (Self : UTF8_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return VSS.Implementation.Strings.Character_Count;

   overriding function Element
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point;
   --  Return character at given position or NUL if Position is not pointing
   --  to any character.

   overriding function Has_Character
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Before_First_Character
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Initialize iterator to point to first character.

   overriding procedure After_Last_Character
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Initialize iterator to point to the last character.

   overriding function Forward
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;
   --  Move cursor one character forward. Return True on success.

   overriding function Backward
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;
   --  Move cursor one character backward. Return True on success.

   overriding procedure From_Wide_Wide_String
     (Self    : in out UTF8_String_Handler;
      Item    : Wide_Wide_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean);

   overriding procedure From_UTF_8_String
     (Self    : in out UTF8_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean);
   --  Convert UTF_8_String into internal representation.

   overriding function To_UTF_8_String
     (Self : UTF8_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Converts string data into standard UTF_8_String.

   overriding procedure Slice
     (Self        : UTF8_String_Handler;
      Source_Data : VSS.Implementation.Strings.String_Data;
      From        : VSS.Implementation.Strings.Cursor;
      To          : VSS.Implementation.Strings.Cursor;
      Target_Data : out VSS.Implementation.Strings.String_Data);

   overriding procedure Append
     (Self   : UTF8_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Append single code point to the data.

   overriding procedure Append
     (Self   : UTF8_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Append another string to the data.

   overriding procedure Insert
     (Self   : UTF8_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Delete
     (Self : UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Split_Lines
     (Self            : UTF8_String_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);
   --  Splits string into lines using given set of allowed new line
   --  terminators. Line terminator (character or combination of characters)
   --  are removed unless Keep_Terminator is set to True.

   overriding procedure Get_Case_Mapping
     (Self    : UTF8_String_Handler;
      Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.String_Handlers.Case_Mapping;
      Data    : out VSS.Implementation.Strings.String_Data);
   --  Fill given case mapping for the given character into Target.

   overriding procedure Convert_Case
     (Self    : UTF8_String_Handler;
      Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.String_Handlers.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data);

   overriding procedure Normalize
     (Self   : UTF8_String_Handler;
      Data   : VSS.Implementation.Strings.String_Data;
      Form   : VSS.Strings.Normalization_Form;
      Result : out VSS.Implementation.Strings.String_Data);

   In_Place_Storage_Capacity : constant := 17;
   --  Number of code units can be stored in place

   subtype In_Place_UTF8_Code_Unit_Count is
     VSS.Unicode.UTF8_Code_Unit_Count range 0 .. In_Place_Storage_Capacity;

   subtype In_Place_Character_Count is
     VSS.Implementation.Strings.Character_Count
   range 0
     .. VSS.Implementation.Strings.Character_Count (In_Place_Storage_Capacity);

   type UTF8_In_Place_Data is record
      Storage :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
          (0 .. In_Place_Storage_Capacity);
      Size    : In_Place_UTF8_Code_Unit_Count;
      Length  : In_Place_Character_Count;
   end record;
   for UTF8_In_Place_Data use record
      Storage at 0 range 0 .. 8 * (In_Place_Storage_Capacity + 1) - 1;
      Size    at In_Place_Storage_Capacity + 1 range 0 .. 7;
      Length  at In_Place_Storage_Capacity + 1 range 8 .. 16;
   end record;

   type UTF8_In_Place_String_Handler is
     new VSS.Implementation.String_Handlers.Abstract_String_Handler
       with null record;

   overriding procedure Reference
     (Self : UTF8_In_Place_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is null;
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   overriding procedure Unreference
     (Self : UTF8_In_Place_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is null;
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

   overriding procedure Initialize
     (Self : UTF8_In_Place_String_Handler;
      Data : out VSS.Implementation.Strings.String_Data);

   overriding function Is_Empty
     (Self : UTF8_In_Place_String_Handler;
      Data : VSS.Implementation.Strings.String_Data) return Boolean;

   overriding function Length
     (Self : UTF8_In_Place_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return VSS.Implementation.Strings.Character_Count;

   overriding function Element
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point;
   --  Return character at given position or NUL if Position is not pointing
   --  to any character.

   overriding function Has_Character
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Before_First_Character
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Initialize iterator to point to first character.

   overriding procedure After_Last_Character
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Initialize iterator to point to the last character.

   overriding function Forward
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;
   --  Move cursor one character forward. Return True on success.

   overriding function Backward
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;
   --  Move cursor one character backward. Return True on success.

   overriding procedure From_Wide_Wide_String
     (Self    : in out UTF8_In_Place_String_Handler;
      Item    : Wide_Wide_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean);

   overriding procedure From_UTF_8_String
     (Self    : in out UTF8_In_Place_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean);
   --  Convert UTF_8_String into internal representation.

   overriding function To_UTF_8_String
     (Self : UTF8_In_Place_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Converts string data into standard UTF_8_String.

   overriding procedure Slice
     (Self        : UTF8_In_Place_String_Handler;
      Source_Data : VSS.Implementation.Strings.String_Data;
      From        : VSS.Implementation.Strings.Cursor;
      To          : VSS.Implementation.Strings.Cursor;
      Target_Data : out VSS.Implementation.Strings.String_Data);

   overriding procedure Append
     (Self   : UTF8_In_Place_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Append single code point to the data.

   overriding procedure Append
     (Self   : UTF8_In_Place_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Append another string to the data.

   overriding procedure Insert
     (Self   : UTF8_In_Place_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Delete
     (Self : UTF8_In_Place_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Split_Lines
     (Self            : UTF8_In_Place_String_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);
   --  Splits string into lines using given set of allowed new line
   --  terminators. Line terminator (character or combination of characters)
   --  are removed unless Keep_Terminator is set to True.

   overriding procedure Get_Case_Mapping
     (Self    : UTF8_In_Place_String_Handler;
      Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.String_Handlers.Case_Mapping;
      Data    : out VSS.Implementation.Strings.String_Data);

   overriding procedure Convert_Case
     (Self    : UTF8_In_Place_String_Handler;
      Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.String_Handlers.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data);

   overriding procedure Normalize
     (Self   : UTF8_In_Place_String_Handler;
      Data   : VSS.Implementation.Strings.String_Data;
      Form   : VSS.Strings.Normalization_Form;
      Result : out VSS.Implementation.Strings.String_Data);

   Global_UTF8_String_Handler   : aliased
     VSS.Implementation.UTF8_String_Handlers.UTF8_String_Handler;

end VSS.Implementation.UTF8_String_Handlers;
