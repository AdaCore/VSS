--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Special case of string handler to process null strings.
--
--  All subprograms of String_Handler must be overridden and all necessary
--  preconditions is added.

package VSS.Implementation.Text_Handlers.Nul is

   pragma Preelaborate;

   pragma Warnings (Off, "bits of ""Null_String_Handler"" unused");
   --  Size of the text handler object is fixed.

   type Null_String_Handler is
     new VSS.Implementation.Text_Handlers.Abstract_String_Handler
       with null record with Object_Size => 192;

   overriding procedure Reference (Self : in out Null_String_Handler);

   overriding procedure Unreference (Self : in out Null_String_Handler);

   overriding function Is_Empty (Self : Null_String_Handler) return Boolean;

   overriding function Is_Null (Self : Null_String_Handler) return Boolean;

   overriding procedure Hash
     (Self      : Null_String_Handler;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator);

   overriding function Length
     (Self : Null_String_Handler)
      return VSS.Implementation.Strings.Character_Count;

   overriding function Element
     (Self     : Null_String_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base;

   overriding function Has_Character
     (Self     : Null_String_Handler;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Before_First_Character
     (Self     : Null_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding procedure After_Last_Character
     (Self     : Null_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding function Forward
     (Self     : Null_String_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean;

   overriding function Backward
     (Self     : Null_String_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;

   --  not overriding function Is_Equal
   --    (Self       : Abstract_String_Handler;
   --     Data       : VSS.Implementation.Strings.String_Data;
   --     Other      : Abstract_String_Handler'Class;
   --     Other_Data : VSS.Implementation.Strings.String_Data) return Boolean;
   --  not overriding function Is_Less
   --    (Self       : Abstract_String_Handler;
   --     Data       : VSS.Implementation.Strings.String_Data;
   --     Other      : Abstract_String_Handler'Class;
   --     Other_Data : VSS.Implementation.Strings.String_Data) return Boolean;
   --  not overriding function Is_Less_Or_Equal
   --    (Self       : Abstract_String_Handler;
   --     Data       : VSS.Implementation.Strings.String_Data;
   --     Other      : Abstract_String_Handler'Class;
   --     Other_Data : VSS.Implementation.Strings.String_Data) return Boolean;

   overriding function Starts_With
     (Self   : Null_String_Handler;
      Prefix : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class)
      return Boolean;

   overriding function Ends_With
     (Self   : Null_String_Handler;
      Suffix : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class)
      return Boolean;

   overriding procedure From_Wide_Wide_String
     (Self    : in out Null_String_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean);

   overriding procedure From_UTF_8_String
     (Self    : in out Null_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean);

   overriding function To_UTF_8_String
     (Self : Null_String_Handler)
      return Ada.Strings.UTF_Encoding.UTF_8_String;

   --  not overriding function First_UTF8_Offset
   --    (Self     : Abstract_String_Handler;
   --     Data     : VSS.Implementation.Strings.String_Data;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --
   --  not overriding function Last_UTF8_Offset
   --    (Self     : Abstract_String_Handler;
   --     Data     : VSS.Implementation.Strings.String_Data;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;
   --
   --  not overriding function First_UTF16_Offset
   --    (Self     : Abstract_String_Handler;
   --     Data     : VSS.Implementation.Strings.String_Data;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --
   --  not overriding function Last_UTF16_Offset
   --    (Self     : Abstract_String_Handler;
   --     Data     : VSS.Implementation.Strings.String_Data;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;
   --  --  Return offset of the last UTF-16 code unit at the given position.
   --
   --  not overriding procedure Compute_Size
   --    (Self   : Abstract_String_Handler;
   --     Data   : VSS.Implementation.Strings.String_Data;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     To     : VSS.Implementation.Strings.Cursor;
   --     Size   : out VSS.Implementation.Strings.Cursor_Offset);
   --  --  Compute size of the given segment. All components of Size have valid
   --  --  and positive values.

   overriding procedure Append
     (Self   : in out Null_String_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Append
     (Self   : in out Null_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Insert
     (Self   : in out Null_String_Handler;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   --  not overriding procedure Insert
   --    (Self   : Abstract_String_Handler;
   --     Data   : in out VSS.Implementation.Strings.String_Data;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     Item   : VSS.Implementation.Strings.String_Data;
   --     Offset : in out VSS.Implementation.Strings.Cursor_Offset);
   --  --  Insert string into the string.
   --  --
   --  --  Implementation must increment value of the Offset.

   overriding procedure Delete
     (Self : in out Null_String_Handler;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   --  not overriding procedure Slice
   --    (Self   : Abstract_String_Handler;
   --     Source : VSS.Implementation.Strings.String_Data;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     To     : VSS.Implementation.Strings.Cursor;
   --     Target : out VSS.Implementation.Strings.String_Data);
   --  --  Return slice of the string.
   --  --  Default implementation construct slice by processing individual
   --  --  characters.

   overriding procedure Split_Lines
     (Self            : Null_String_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

end VSS.Implementation.Text_Handlers.Nul;
