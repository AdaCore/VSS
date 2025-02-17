--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic implementation of the constant text which use UTF-8 encoding.

with VSS.Implementation.Interfaces_C;
with VSS.Implementation.UTF8_Encoding;

package VSS.Implementation.Text_Handlers.UTF8
  with Preelaborate
is

   type Abstract_UTF8_Text is
     abstract new VSS.Implementation.Text_Handlers.Abstract_Text_Handler
       with null record;
   --  This type provides direct access to underlying text storage and its
   --  size. It implements some operations that doesn't require modifications
   --  of the text.

   not overriding function UTF8_Size
     (Self : Abstract_UTF8_Text) return VSS.Unicode.UTF8_Code_Unit_Count
        is abstract;
   --  Return number of code units in the given text

   not overriding function UTF8_Constant_Storage_Poiner
     (Self : Abstract_UTF8_Text)
      return not null
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access
          is abstract;
   --  Returns pointer to the first element in the text data storage.

   not overriding procedure UTF8_Constant_Storage_And_Size
     (Self    : Abstract_UTF8_Text;
      Pointer : out
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : out VSS.Unicode.UTF8_Code_Unit_Count) is abstract;
   --  Returns pointer to the first element of the text storage and size of the
   --  storage.

private

   procedure Unchecked_Backward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Move cursor to position of the previous character

   procedure Unchecked_Forward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor)
        with Inline_Always;
   --  Move cursor to position of the next character

end VSS.Implementation.Text_Handlers.UTF8;
