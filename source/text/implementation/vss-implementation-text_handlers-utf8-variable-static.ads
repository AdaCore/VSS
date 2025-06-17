--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  UTF-8 encoded text and static storage

pragma Ada_2022;

package VSS.Implementation.Text_Handlers.UTF8.Variable.Static is

   pragma Preelaborate;

   In_Place_Storage_Capacity : constant := 16 - 1;
   --  Number of code units can be stored in place

   type Static_UTF8_Handler is new Variable_UTF8_Text with record
      Storage :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
          (0 .. In_Place_Storage_Capacity) := [others => 0];
   end record with Pack, Object_Size => 256;

   overriding procedure Reference
     (Self : in out Static_UTF8_Handler) is null;

   overriding procedure Unreference
     (Self : in out Static_UTF8_Handler) is null;

   overriding function Is_Empty
     (Self : Static_UTF8_Handler) return Boolean;

   overriding function Element
     (Self     : Static_UTF8_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base;

   overriding function Has_Character
     (Self     : Static_UTF8_Handler;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Before_First_Character
     (Self     : Static_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding procedure After_Last_Character
     (Self     : Static_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding function Forward
     (Self     : Static_UTF8_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean;

   overriding function Forward_Element
     (Self     : Static_UTF8_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean;

   overriding function Backward
     (Self     : Static_UTF8_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure From_Wide_Wide_String
     (Self    : in out Static_UTF8_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean);

   overriding procedure From_UTF_8_String
     (Self    : in out Static_UTF8_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean);

   overriding function To_UTF_8_String
     (Self : Static_UTF8_Handler)
      return Ada.Strings.UTF_Encoding.UTF_8_String;

   overriding procedure Slice
     (Self        : Static_UTF8_Handler;
      From        : VSS.Implementation.Strings.Cursor;
      To          : VSS.Implementation.Strings.Cursor;
      Target_Data : out VSS.Implementation.Strings.String_Data);

   overriding procedure Append
     (Self   : in out Static_UTF8_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Append
     (Self   : in out Static_UTF8_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Insert
     (Self   : in out Static_UTF8_Handler;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Delete
     (Self : in out Static_UTF8_Handler;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Split_Lines
     (Self            : Static_UTF8_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   overriding procedure UTF8_Insert_Slice
     (Self    : in out Static_UTF8_Handler;
      Into    : VSS.Unicode.UTF8_Code_Unit_Index;
      Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Length  : VSS.Implementation.Strings.Character_Count);

   overriding procedure UTF8_Move
     (Self : in out Static_UTF8_Handler;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index);

   overriding procedure UTF8_Replace_Slice
     (Self           : in out Static_UTF8_Handler;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      By_Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      By_From        : VSS.Unicode.UTF8_Code_Unit_Index;
      By_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      By_Length      : VSS.Implementation.Strings.Character_Count);

end VSS.Implementation.Text_Handlers.UTF8.Variable.Static;
