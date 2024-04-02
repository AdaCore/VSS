--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic implementation of the string which use UTF-8 encoding for data.

pragma Ada_2022;

with Ada.Strings.UTF_Encoding;
with System.Atomic_Counters;

with VSS.Implementation.Text_Handlers;
with VSS.Implementation.String_Vectors;
with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Encoding;
with VSS.Strings;
with VSS.Unicode;

package VSS.Implementation.UTF8_String_Handlers is

   pragma Preelaborate;

   type UTF8_String_Data (Bulk : VSS.Unicode.UTF8_Code_Unit_Count) is record
      Counter : System.Atomic_Counters.Atomic_Counter;

      Size    : VSS.Unicode.UTF8_Code_Unit_Count           := 0;
      --  Number of code units in the buffer.

      Length  : VSS.Implementation.Strings.Character_Count := 0;
      --  Length of the string in Unicode Code Points.

      Storage :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array (0 .. Bulk);
      --  Buffer to store string's data. First unused code unit is set to
      --  zero, to allow to pass data to C.
   end record;

   type UTF8_String_Data_Access is access all UTF8_String_Data;

   type UTF8_String_Handler is
     new VSS.Implementation.Text_Handlers.Abstract_String_Handler with
   record
      Pointer : UTF8_String_Data_Access;
   end record with Object_Size => 192;

   overriding procedure Reference
     (Self : in out UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data);
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   overriding procedure Unreference
     (Self : in out UTF8_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data);
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

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
      return VSS.Unicode.Code_Point'Base;
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

   overriding function Forward_Element
     (Self     : UTF8_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean;

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
      Success : out Boolean);

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
     (Self   : in out UTF8_String_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Append
     (Self   : in out UTF8_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Append another string to the data.

   overriding procedure Insert
     (Self   : in out UTF8_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Delete
     (Self : in out UTF8_String_Handler;
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

   In_Place_Storage_Capacity : constant := 16 - 2 - 1;
   --  Number of code units can be stored in place

   subtype In_Place_UTF8_Code_Unit_Count is
     VSS.Unicode.UTF8_Code_Unit_Count range 0 .. In_Place_Storage_Capacity;

   subtype In_Place_Character_Count is
     VSS.Implementation.Strings.Character_Count
       range 0
         .. VSS.Implementation.Strings.Character_Count
              (In_Place_Storage_Capacity);

   type UTF8_In_Place_String_Handler is
     new VSS.Implementation.Text_Handlers.Abstract_String_Handler with
   record
      Storage :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
          (0 .. In_Place_Storage_Capacity) := [others => 0];
      Size    : In_Place_UTF8_Code_Unit_Count := 0;
      Length  : In_Place_Character_Count      := 0;
   end record with Pack, Object_Size => 192;

   overriding procedure Reference
     (Self : in out UTF8_In_Place_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is null;
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   overriding procedure Unreference
     (Self : in out UTF8_In_Place_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is null;
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

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
      return VSS.Unicode.Code_Point'Base;
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

   overriding function Forward_Element
     (Self     : UTF8_In_Place_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean;

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
      Success : out Boolean);

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
     (Self   : in out UTF8_In_Place_String_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Append
     (Self   : in out UTF8_In_Place_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  Append another string to the data.

   overriding procedure Insert
     (Self   : in out UTF8_In_Place_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Delete
     (Self : in out UTF8_In_Place_String_Handler;
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

   Global_UTF8_String_Handler   : aliased
     VSS.Implementation.UTF8_String_Handlers.UTF8_String_Handler;

   --  Subprograms to help code refactoring, some of the will be moved to
   --  generic UTF8 fastpath string API, and some moved to the body after
   --  that.

   procedure Convert_To_Dynamic
     (Text     : in out UTF8_In_Place_String_Handler;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Convert static text data into a dynamically allocated one.
   --  Use expected Capacity and Size to allocate a storage block, then copy
   --  text content to the allocated block.

   function Allocate
     (Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count)
      return UTF8_String_Data_Access;
   --  Allocate storage block to store at least given amount of the data.

   procedure Reallocate
     (Data     : in out UTF8_String_Data_Access;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Reallocates storage block to store at least given amount of the data.
   --  Content of the data will be copied, and old storage block will be
   --  unreferenced (and deallocated if it is no longer used).

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False);
   --  Append given slice of the data to the target. Convert target
   --  from in-place to heap based implementation when necessary.

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False);
   --  Append given slice of the data to the target. Convert target
   --  from in-place to heap based implementation when necessary.

   procedure Unsafe_Initialize
     (Text     : in out
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Capacity : VSS.Implementation.Strings.Character_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Initialize UTF-8 text storage. It is either static or dynamic,
   --  depending from the requested capacity and/or size, which is larger.
   --
   --  Memory for Text object is used to initialize new object, thus any
   --  content is destroyed, and "type" of the object will change.

end VSS.Implementation.UTF8_String_Handlers;
