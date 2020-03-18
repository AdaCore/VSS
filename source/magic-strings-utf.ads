------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--  Utilities to handle strings as code units in UTF-family encodings.
--
--  This package is intended to be used by relatively low level code.
--
--  Opposite to general Ada convention to use '1'-based indexing of the
--  characters in the strings, here '0'-based indexing is used as more
--  appropriate for low level applications and interoparability.

package Magic.Strings.UTF is

   pragma Preelaborate;

   type UTF8_Code_Unit is mod 2 ** 8;
   type UTF8_Code_Unit_Count is new Natural;
   subtype UTF8_Code_Unit_Index is UTF8_Code_Unit_Count;

   type UTF16_Code_Unit is mod 2 ** 16;
   type UTF16_Code_Unit_Count is new Natural;
   subtype UTF16_Code_Unit_Index is UTF16_Code_Unit_Count;

   type UTF32_Code_Unit is mod 2 ** 32; -- range 0 .. 16#10_FFFF#;
   type UTF32_Code_Unit_Count is new Natural;
   subtype UTF32_Code_Unit_Index is UTF32_Code_Unit_Count;

   function To_Grapheme_Index
     (Item : UTF16_Code_Unit_Index) return Grapheme_Index;
   --  Converts index of the UTF16 encoded data to index of the grapheme.

   function To_UTF16_Code_Unit_Count
     (Item : Grapheme_Index) return UTF16_Code_Unit_Index;
   --  Converts index of the grapheme in the string to index of the UTF16
   --  encoded data.

end Magic.Strings.UTF;
