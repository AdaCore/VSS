--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Mutable operations on internal representation of UTF-8 encoded text.

with VSS.Implementation.UTF8_Encoding;

package VSS.Implementation.UTF8_Strings.Mutable_Operations
  with Preelaborate
is

   use type System.Address;

   procedure Append
     (Text   : in out UTF8_String_Data;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   procedure Append
     (Text : in out UTF8_String_Data;
      Code : VSS.Unicode.Code_Point);

   procedure Append
     (Text   : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Suffix : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   procedure Append
     (Text        : in out UTF8_String_Data;
      Item_Data   : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Item_Length : VSS.Implementation.Strings.Character_Count);

   procedure Insert
     (Text   : in out UTF8_String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   procedure Insert
     (Text   : in out UTF8_String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   procedure Delete
     (Text : in out UTF8_String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   procedure From_UTF_8_String
     (Self    : in out UTF8_String_Data;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean);

   procedure From_Wide_Wide_String
     (Self    : in out UTF8_String_Data;
      Item    : Wide_Wide_String;
      Success : out Boolean);

   procedure Initialize
     (Text     : in out UTF8_String_Data;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count)
     with Pre => Text.Storage_Address = System.Null_Address;
   --  Initialize text object and preserve given capacity

   procedure Initialize
     (Text   : in out UTF8_String_Data;
      Data   : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length : VSS.Implementation.Strings.Character_Count)
     with Pre => Text.Storage_Address = System.Null_Address;
   --  Initialize text object by given data

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False);
   --  Append given slice of the data to the target. Convert target
   --  from in-place to heap based implementation when necessary.

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False);
   --  Append given slice of the data to the target. Convert target
   --  from in-place to heap based implementation when necessary.

   procedure Unchecked_Delete
     (Target_Data   : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size   : out VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Delete_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_Length : VSS.Implementation.Strings.Character_Count);
   --  Delete given slice of the data from the target starting from the given
   --  position.

   procedure Unchecked_Insert
     (Target_Text    : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
      Into           : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Insert_From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Insert_Length  : VSS.Implementation.Strings.Character_Count);
   --  Insert given slice of the data into the target starting from the given
   --  position. Convert target from in-place to heap based implementation
   --  when necessary.

   procedure Unchecked_Replace
     (Target_Data    : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Target_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      Insert_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Insert_From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Insert_Length  : VSS.Implementation.Strings.Character_Count);
   --  Replace given slice of the target data by another data. Convert target
   --  from in-place to heap based implementation when necessary.

   procedure Unchecked_Move_Slice
     (Text : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index);
   --  Move given slice of the give size of the data starting from the given
   --  position. From and into positions must be valid positions in UTF-8
   --  encoded data, thus size and length of the string is not changed by
   --  this operation.

end VSS.Implementation.UTF8_Strings.Mutable_Operations;
