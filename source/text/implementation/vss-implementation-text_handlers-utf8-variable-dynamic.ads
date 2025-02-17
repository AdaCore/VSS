--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  UTF-8 encoded text with dynamic (on heap) storage.

with System.Atomic_Counters;

package VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic is

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

   pragma Warnings (Off, "bits of ""Dynamic_UTF8_Handler"" unused");
   --  Size of the text handler object is fixed.

   type Dynamic_UTF8_Handler is new Variable_UTF8_Text with record
      Pointer : UTF8_String_Data_Access;
   end record with Object_Size => 192;

   overriding procedure Reference (Self : in out Dynamic_UTF8_Handler)
     with Pre => Self.Pointer /= null;

   overriding procedure Unreference (Self : in out Dynamic_UTF8_Handler)
     with Post => Self.Pointer = null;

   overriding function Is_Empty (Self : Dynamic_UTF8_Handler) return Boolean
     with Pre => Self.Pointer /= null;

   overriding function Length
     (Self : Dynamic_UTF8_Handler)
      return VSS.Implementation.Strings.Character_Count
     with Pre => Self.Pointer /= null;

   overriding function Element
     (Self     : Dynamic_UTF8_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base
     with Pre => Self.Pointer /= null;

   overriding function Has_Character
     (Self     : Dynamic_UTF8_Handler;
      Position : VSS.Implementation.Strings.Cursor) return Boolean
     with Pre => Self.Pointer /= null;

   overriding procedure Before_First_Character
     (Self     : Dynamic_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor)
     with Pre => Self.Pointer /= null;

   overriding procedure After_Last_Character
     (Self     : Dynamic_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor)
     with Pre => Self.Pointer /= null;

   overriding function Forward
     (Self     : Dynamic_UTF8_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean
     with Pre => Self.Pointer /= null;

   overriding function Forward_Element
     (Self     : Dynamic_UTF8_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean
     with Pre => Self.Pointer /= null;

   overriding function Backward
     (Self     : Dynamic_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean
     with Pre => Self.Pointer /= null;

   overriding procedure From_Wide_Wide_String
     (Self    : in out Dynamic_UTF8_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean)
     with Pre => Self.Pointer /= null;

   overriding procedure From_UTF_8_String
     (Self    : in out Dynamic_UTF8_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean)
     with Pre => Self.Pointer /= null;

   overriding function To_UTF_8_String
     (Self : Dynamic_UTF8_Handler)
      return Ada.Strings.UTF_Encoding.UTF_8_String
     with Pre => Self.Pointer /= null;

   overriding procedure Slice
     (Self        : Dynamic_UTF8_Handler;
      From        : VSS.Implementation.Strings.Cursor;
      To          : VSS.Implementation.Strings.Cursor;
      Target_Data : out VSS.Implementation.Strings.String_Data)
     with Pre => Self.Pointer /= null;

   overriding procedure Append
     (Self   : in out Dynamic_UTF8_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
     with Pre => Self.Pointer /= null;

   overriding procedure Append
     (Self   : in out Dynamic_UTF8_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
     with Pre => Self.Pointer /= null;

   overriding procedure Insert
     (Self   : in out Dynamic_UTF8_Handler;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
     with Pre => Self.Pointer /= null;

   overriding procedure Delete
     (Self : in out Dynamic_UTF8_Handler;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset)
     with Pre => Self.Pointer /= null;

   overriding procedure Split_Lines
     (Self            : Dynamic_UTF8_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
     with Pre => Self.Pointer /= null;

   overriding procedure UTF8_Insert_Slice
     (Self    : in out Dynamic_UTF8_Handler;
      Into    : VSS.Unicode.UTF8_Code_Unit_Index;
      Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Length  : VSS.Implementation.Strings.Character_Count);

   overriding procedure UTF8_Move
     (Self : in out Dynamic_UTF8_Handler;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index);

   overriding procedure UTF8_Replace_Slice
     (Self           : in out Dynamic_UTF8_Handler;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      By_Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      By_From        : VSS.Unicode.UTF8_Code_Unit_Index;
      By_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      By_Length      : VSS.Implementation.Strings.Character_Count);

   overriding function UTF8_Size
     (Self : Dynamic_UTF8_Handler) return VSS.Unicode.UTF8_Code_Unit_Count;

   overriding function UTF8_Constant_Storage_Poiner
     (Self : Dynamic_UTF8_Handler)
      return not null
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;

   overriding procedure UTF8_Constant_Storage_And_Size
     (Self    : Dynamic_UTF8_Handler;
      Pointer : out
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : out VSS.Unicode.UTF8_Code_Unit_Count);

   --  Subprograms to help code refactoring, some of the will be moved to
   --  generic UTF8 fastpath string API, and some moved to the body after
   --  that.

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

end VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic;
