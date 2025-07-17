--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic implementation of the constant text which use UTF-8 encoding.

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

   overriding procedure Append
     (Self   : in out UTF8_Text;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding function Backward
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Before_First_Character
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor);

   --  overriding procedure Compute_Size
   --    (Self   : UTF8_Text;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     To     : VSS.Implementation.Strings.Cursor;
   --     Size   : out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Delete
     (Self : in out UTF8_Text;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   --  overriding function Ends_With
   --    (Self   : UTF8_Text;
   --     Suffix : Abstract_Text_Handler'Class) return Boolean;

   overriding function Element
     (Self     : UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base;

   --  overriding function First_UTF8_Offset
   --    (Self     : UTF8_Text;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;

   --  overriding function First_UTF16_Offset
   --    (Self     : UTF8_Text;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Forward
     (Self     : UTF8_Text;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean;

   overriding function Forward_Element
     (Self     : UTF8_Text;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean;

   overriding procedure From_UTF_8_String
     (Self    : in out UTF8_Text;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean);

   overriding procedure From_Wide_Wide_String
     (Self    : in out UTF8_Text;
      Item    : Wide_Wide_String;
      Success : out Boolean);

   overriding function Has_Character
     (Self     : UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;

   --  overriding procedure Hash
   --    (Self      : UTF8_Text;
   --     Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator);

   overriding procedure Insert
     (Self   : in out UTF8_Text;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   --  overriding procedure Insert
   --    (Self   : in out UTF8_Text;
   --     From   : VSS.Implementation.Strings.Cursor;
   --     Item   : VSS.Implementation.Strings.String_Data;
   --     Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding function Is_Empty (Self : UTF8_Text) return Boolean;

   overriding function Is_Equal
     (Self  : UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean;

   overriding function Is_Less
     (Self  : UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean;

   overriding function Is_Less_Or_Equal
     (Self  : UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean;

   overriding function Is_Null (Self : UTF8_Text) return Boolean;

   --  overriding function Last_UTF8_Offset
   --    (Self     : UTF8_Text;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF8_Code_Unit_Index;

   --  overriding function Last_UTF16_Offset
   --    (Self     : UTF8_Text;
   --     Position : VSS.Implementation.Strings.Cursor)
   --     return VSS.Unicode.UTF16_Code_Unit_Index;

   overriding function Length
     (Self : UTF8_Text) return VSS.Implementation.Strings.Character_Count;

   overriding procedure Reference (Self : in out UTF8_Text);

   overriding procedure Slice
     (Self   : UTF8_Text;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Target : out VSS.Implementation.Strings.String_Data);

   --  overriding procedure Split
   --    (Self             : UTF8_Text;
   --     Data             : VSS.Implementation.Strings.String_Data;
   --     Separator        : VSS.Unicode.Code_Point;
   --     Keep_Empty_Parts : Boolean;
   --     Items            : in out
   --       VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   overriding procedure Split_Lines
     (Self            : UTF8_Text;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   --  overriding function Starts_With
   --    (Self   : UTF8_Text;
   --     Prefix : Abstract_Text_Handler'Class) return Boolean;

   overriding function To_UTF_8_String
     (Self : UTF8_Text) return Ada.Strings.UTF_Encoding.UTF_8_String;

   overriding procedure Unreference (Self : in out UTF8_Text);

end VSS.Implementation.Text_Handlers.UTF8;
