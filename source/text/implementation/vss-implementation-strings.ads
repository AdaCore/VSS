--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Unicode;

package VSS.Implementation.Strings
  with Preelaborate
is

   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;

   type Character_Offset is range -2 ** 30 .. 2 ** 30 - 1;
   subtype Character_Count is Character_Offset
     range 0 .. Character_Offset'Last;
   subtype Character_Index is Character_Count range 1 .. Character_Count'Last;

   type Grapheme_Count is range 0 .. 2 ** 30 - 1;
   subtype Grapheme_Index is Grapheme_Count range 1 .. Grapheme_Count'Last;

   No_Character : constant VSS.Unicode.Code_Point_Unit :=
     VSS.Unicode.Code_Point_Unit'Last;
   --  Special value to return when there is no character at given position.

   ------------
   -- Cursor --
   ------------

   type Cursor is record
      UTF8_Offset  : VSS.Unicode.UTF8_Code_Unit_Offset  :=
        VSS.Unicode.UTF8_Code_Unit_Offset'Last;
      UTF16_Offset : VSS.Unicode.UTF16_Code_Unit_Offset :=
        VSS.Unicode.UTF16_Code_Unit_Offset'Last;
      Index        : Character_Count                    := 0;
   end record;
   --  Position of the character in the string. There are few special values,
   --  see table below.
   --
   --  UTF8_Offset and UTF16_Offset may be negative value, it means that
   --  actual offset is not know and can be computed by subtraction of this
   --  value from the total length of the data in corresponding encoding. It
   --  is used to avoid scanning of the whole string when cursor for the last
   --  character of the string is constructed.
   --
   --  Some special corner cases for values:
   --
   --                                   Index      UTF8_Offset   UTF16_Offset
   --   - invalid position                0           'Last         'Last
   --   - before first character          0             -1            -1
   --   - after last character        Length + 1     0 | Size      0 | Size
   --
   --  UTF8_Offset and UTF16_Offset components are put into the beginning to
   --  allow compiler to optimize operations on them with SIMD instructions.

   Position_Before_First_Character : constant
     VSS.Implementation.Strings.Cursor :=
       (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);

   function Is_Invalid (Self : Cursor) return Boolean;
   --  Return True when cursor has special invalid value.

   -------------------
   -- Cursor_Offset --
   -------------------

   type Cursor_Offset is record
      UTF8_Offset  : VSS.Unicode.UTF8_Code_Unit_Offset  := 0;
      UTF16_Offset : VSS.Unicode.UTF16_Code_Unit_Offset := 0;
      Index_Offset : Character_Offset                   := 0;
   end record;
   --  Offset between positions of two Cursors. Also used as size of the
   --  segment.
   --
   --  Order of components is same with order of components of Cursor type.

   procedure Fixup_Insert
     (Self  : in out Cursor;
      Start : Cursor;
      Size  : Cursor_Offset);
   --  Fixup position of the cursor on insert operation at the given position
   --  and size.

   function Fixup_Delete
     (Self  : in out Cursor;
      Start : Cursor;
      Size  : Cursor_Offset) return Boolean;
   --  Fixup position of the cursor on delete operation at the given position
   --  and size. Return False and set position to invalid value when position
   --  of the cursor has been deleted.

end VSS.Implementation.Strings;
