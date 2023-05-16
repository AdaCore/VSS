--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Utilities to handle strings as code units in UTF-family encodings.
--
--  This package is intended to be used by relatively low level code.
--
--  Opposite to general Ada convention to use '1'-based indexing of the
--  characters in the strings, here '0'-based indexing is used as more
--  appropriate for low level applications and interoperability.

with Interfaces;

package VSS.Unicode is

   pragma Pure;

   type Code_Point is
     new Interfaces.Unsigned_32 range 16#00_0000# .. 16#10_FFFF#;

   type UTF8_Code_Unit is mod 2 ** 8;
   type UTF8_Code_Unit_Offset is new Interfaces.Integer_32;
   subtype UTF8_Code_Unit_Count is
     UTF8_Code_Unit_Offset range 0 .. UTF8_Code_Unit_Offset'Last;
   subtype UTF8_Code_Unit_Index is UTF8_Code_Unit_Count;

   type UTF16_Code_Unit is mod 2 ** 16;
   type UTF16_Code_Unit_Offset is new Interfaces.Integer_32;
   subtype UTF16_Code_Unit_Count is
     UTF16_Code_Unit_Offset range 0 .. UTF16_Code_Unit_Offset'Last;
   subtype UTF16_Code_Unit_Index is UTF16_Code_Unit_Count;

   type UTF32_Code_Unit is mod 2 ** 32; -- range 0 .. 16#10_FFFF#;
   type UTF32_Code_Unit_Offset is new Interfaces.Integer_32;
   subtype UTF32_Code_Unit_Count is
     UTF32_Code_Unit_Offset range 0 .. UTF32_Code_Unit_Offset'Last;
   subtype UTF32_Code_Unit_Index is UTF32_Code_Unit_Count;

   subtype Code_Point_Character is Wide_Wide_Character
     range Wide_Wide_Character'Val (16#00_0000#)
             .. Wide_Wide_Character'Val (16#10_FFFF#);
   --  Limits possible values to the range of the Unicode Code Points: any code
   --  with value in range 16#00_0000# .. 16#10_FFFF#.

   subtype Scalar_Value_Character is Code_Point_Character
     with Static_Predicate =>
       Scalar_Value_Character
         not in Code_Point_Character'Val (16#D800#)
                  .. Code_Point_Character'Val (16#DF00#);
   --  Limits possible values to the range of the Unicode Scalar Values: any
   --  Unicode code points outside of the surrogate range (16#00_D800# ..
   --  16#DFFF#).

end VSS.Unicode;
