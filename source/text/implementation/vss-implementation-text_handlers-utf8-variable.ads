--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic implementation of the variable text which use UTF-8 encoding.

with VSS.Implementation.UTF8_Encoding;

limited private with VSS.Implementation.Text_Handlers.UTF8.Variable.Static;

package VSS.Implementation.Text_Handlers.UTF8.Variable
  with Preelaborate
is

   type Variable_UTF8_Text is
     abstract new Abstract_UTF8_Text with null record;

   not overriding procedure UTF8_Replace_Slice
     (Self           : in out Variable_UTF8_Text;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      By_Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      By_From        : VSS.Unicode.UTF8_Code_Unit_Index;
      By_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      By_Length      : VSS.Implementation.Strings.Character_Count) is abstract;
   --  Replace slice of the text by slice of the storage.
   --
   --  @param Self            Text object to be modified
   --  @param Replace_From    Index of the first code unit to replace
   --  @param Replace_Size    Number of code units to replace
   --  @param Replace_Length  Number of characters to be replaced
   --  @param By_Storage      Storage of the replacement data
   --  @param By_From         Index of the first code unit in replcement data
   --  @param By_Size         Number of code units in replacement data
   --  @param by_Length       Number of character in replacement data

   not overriding procedure UTF8_Insert_Slice
     (Self    : in out Variable_UTF8_Text;
      Into    : VSS.Unicode.UTF8_Code_Unit_Index;
      Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Length  : VSS.Implementation.Strings.Character_Count) is abstract;
   --  Insert slice of the storage into the text starting from the given
   --  position.
   --
   --  @param Self     Text object to be modified
   --  @param Into     Index of the code unit to insert
   --  @param Storage  Storeage of inserted data
   --  @param From     Index of the first code unit in inserted storage
   --  @param Size     Number of code units to insert
   --  @param Length   Number of character to insert

   not overriding procedure UTF8_Move
     (Self : in out Variable_UTF8_Text;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index) is abstract;
   --  Move given slice of the give size of the data starting from the given
   --  position. From and Into positions must be valid positions in UTF-8
   --  encoded data, thus size and length of the string is not changed by
   --  this operation.

   procedure Unsafe_Initialize
     (Text : in out
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class;
      Size : VSS.Unicode.UTF8_Code_Unit_Count);
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
     (Text : in out
        VSS.Implementation.Text_Handlers.UTF8.Variable.Static
          .Static_UTF8_Handler;
      Size : VSS.Unicode.UTF8_Code_Unit_Count);
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

   procedure Validate_And_Copy
     (Source      : Ada.Strings.UTF_Encoding.UTF_8_String;
      Destination : out VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Length      : out VSS.Implementation.Strings.Character_Count;
      Success     : out Boolean);
   --  Validate UTF-8 encoding and copy validated part of the data to
   --  Destination. Length is set to the length of the text in characters.
   --  Success is set False when validation is failed and to True otherwise.

end VSS.Implementation.Text_Handlers.UTF8.Variable;
