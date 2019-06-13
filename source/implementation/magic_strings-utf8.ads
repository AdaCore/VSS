------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
--  Generic implementation of the string which use UTF-8 encoding for data.

with Ada.Strings.UTF_Encoding;

with Magic_Strings.Reference_Counted;
with Magic_Strings.UTF;

private package Magic_Strings.UTF8 is

   pragma Preelaborate;

   type UTF8_Code_Unit_Array is
     array (UTF.UTF8_Code_Unit_Count range <>) of UTF.UTF8_Code_Unit;

   type UTF8_Segment (Capacity : UTF.UTF8_Code_Unit_Count) is
     new Magic_Strings.Reference_Counted.Abstract_Shared_String with record
      Data   : UTF8_Code_Unit_Array (0 .. Capacity);
      --  Buffer to store string's data. First unused code unit is set to
      --  zero, to allow to pass data to C.

      Size   : UTF.UTF8_Code_Unit_Count;
      --  Number of code units in the buffer.

      Length : Character_Count;
      --  Length of the string in Unicode Code Points.
   end record;

   procedure From_UTF_8_String
     (Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Segment : out String_Access;
      Success : out Boolean);
   --  Converts standard UTF_S_String into internal representation. It checks
   --  for validity and computes string length in code points. On any error
   --  Success is set to False and Segment set to null.

   overriding function Is_Empty (Self : UTF8_Segment) return Boolean;
   --  Return True when string is empty.

   overriding function To_Text (Self : in out UTF8_Segment) return String_Access;
   --  Returns text view of the segment.

   overriding function To_UTF_8_String (Self : UTF8_Segment) return String;
   --  Returns internal data as standard String.

   type UTF8_Text is
     new Magic_Strings.Reference_Counted.Abstract_Shared_String with null record;

   overriding function Is_Empty (Self : UTF8_Text) return Boolean;
   --  Return True when string is empty.

   overriding function To_Text (Self : in out UTF8_Text) return String_Access;
   --  Returns itself.

   overriding function To_UTF_8_String (Self : UTF8_Text) return String;
   --  Returns internal data as standard String.

end Magic_Strings.UTF8;
