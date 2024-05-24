--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic implementation of the string which use UTF-8 encoding for data.

with VSS.Implementation.UTF8_Encoding;

limited private with VSS.Implementation.Text_Handlers.UTF8.Static;

package VSS.Implementation.Text_Handlers.UTF8
  with Preelaborate
is

   procedure Unsafe_Initialize
     (Text     : in out
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class;
      Capacity : VSS.Implementation.Strings.Character_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Initialize UTF-8 text storage. It is either static or dynamic,
   --  depending from the requested capacity and/or size, which is larger.
   --
   --  Memory for Text object is used to initialize new object, thus any
   --  content is destroyed, and "type" of the object will change.

   --  Subprograms to help code refactoring, some of the will be moved to
   --  generic UTF8 fastpath string API, and some moved to the body after
   --  that.

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

private

   use type VSS.Unicode.UTF8_Code_Unit_Offset;

   procedure Unsafe_Convert_To_Dynamic
     (Text     : in out
        VSS.Implementation.Text_Handlers.UTF8.Static.Static_UTF8_Handler;
      Capacity : VSS.Unicode.UTF8_Code_Unit_Count;
      Size     : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Convert static text data into a dynamically allocated one.
   --  Use expected Capacity and Size to allocate a storage block, then copy
   --  text content to the allocated block.

   procedure Internal_Append
     (Storage        : in out
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length         : in out VSS.Implementation.Strings.Character_Count;
      Size           : in out VSS.Unicode.UTF8_Code_Unit_Count;
      Suffix_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Suffix_Length  : VSS.Implementation.Strings.Character_Count;
      Suffix_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Offset         : in out VSS.Implementation.Strings.Cursor_Offset)
     with Pre => Storage'Last >= Size + Suffix_Size;
   --  Append one storage to another.

   procedure Split_Lines_Common
     (Text            :
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class;
      Data            : VSS.Implementation.Strings.String_Data;
      Storage         : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);
   --  Common code of Split_Lines subprogram for on heap and inline handlers.

   procedure Unchecked_Backward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Move cursor to position of the previous character

   procedure Unchecked_Forward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor)
        with Inline_Always;
   --  Move cursor to position of the next character

   procedure Validate_And_Copy
     (Source      : Ada.Strings.UTF_Encoding.UTF_8_String;
      Destination : out VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length      : out VSS.Implementation.Strings.Character_Count;
      Success     : out Boolean);
   --  Validate UTF-8 encoding and copy validated part of the data to
   --  Destination. Length is set to the length of the text in characters.
   --  Success is set False when validation is failed and to True otherwise.

end VSS.Implementation.Text_Handlers.UTF8;
