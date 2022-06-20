--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
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

end VSS.Unicode;
