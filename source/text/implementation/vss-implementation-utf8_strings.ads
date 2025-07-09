--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Internal representation of UTF-8 encoded text.
--
--  Note, UTF8 encoded text must be valid (shortest encoding form, no
--  surrogates), otherwise optimized operations might return incorrect result.

pragma Ada_2022;

with Ada.Strings.UTF_Encoding;
with Interfaces;
with System.Storage_Elements;

with VSS.Implementation.Strings;
with VSS.Implementation.String_Vectors;
with VSS.Strings;
with VSS.Unicode;

package VSS.Implementation.UTF8_Strings
  with Preelaborate
is

   type UTF8_String_Data is record
      Manager         : System.Storage_Elements.Storage_Array (0 .. 15) :=
        [others => 0];
      Flags           : Interfaces.Unsigned_32                          := 0;
      Size            : VSS.Unicode.UTF8_Code_Unit_Count                := 0;
      --  Number of code units in the buffer.
      Storage_Address : System.Address                                  :=
        System.Null_Address;
      Length          : VSS.Implementation.Strings.Character_Count      := 0;
      --  Length of the string in Unicode Code Points.
   end record with Preelaborable_Initialization;
      --  Alignment   => 8,
      --      Object_Size => 256;

   procedure Reference (Self : in out UTF8_String_Data);

   procedure Unreference (Self : in out UTF8_String_Data);

   procedure Unchecked_Forward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
        with Inline_Always;
   --  Move cursor to position of the next character

   procedure Unchecked_Backward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor);
   --  Move cursor to position of the previous character

   procedure Unchecked_Backward_Decode
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point);
   --  Change offset to the point of the previous character and decode
   --  character at this position.

   procedure Unchecked_Decode_Forward
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point);
   --  Decode UTF8 encoded character started at given offset and change offset
   --  to point to the beginning of the next character.

   function Element
     (Self     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base;

   function To_UTF_8_String
     (Self : UTF8_String_Data) return Ada.Strings.UTF_Encoding.UTF_8_String;

   procedure Split_Lines
     (Text            : UTF8_String_Data;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

private

   SSO_Max_Size : constant := 15;
   --  XXX 64bit only !!!

   function Is_SSO (Self : UTF8_String_Data) return Boolean;

end VSS.Implementation.UTF8_Strings;
