------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
--  Generic implementation of the string which use UTF-8 encoding for data.

with Ada.Strings.UTF_Encoding;

with Magic.Strings.Reference_Counted;

private package Magic.Strings.UTF8 is

   pragma Preelaborate;

   type UTF8_Code_Unit_Array is
     array (Magic.Unicode.UTF8_Code_Unit_Count range <>)
     of Magic.Unicode.UTF8_Code_Unit;

   type UTF8_Segment (Capacity : Magic.Unicode.UTF8_Code_Unit_Count) is
     new Magic.Strings.Reference_Counted.Abstract_Shared_String with record
      Data   : UTF8_Code_Unit_Array (0 .. Capacity);
      --  Buffer to store string's data. First unused code unit is set to
      --  zero, to allow to pass data to C.

      Size   : Magic.Unicode.UTF8_Code_Unit_Count;
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

   overriding procedure First_Character
     (Self     : UTF8_Segment;
      Position : in out Cursor);
   --  Initialize iterator to point to first character of the string

   overriding function Element
     (Self     : UTF8_Segment;
      Position : Cursor) return Magic.Unicode.Code_Point;

   overriding function Forward
     (Self     : UTF8_Segment;
      Position : in out Cursor) return Boolean;
   --  Move cursor one character forward. Return True on success.

   overriding function To_Text (Self : in out UTF8_Segment) return String_Access;
   --  Returns text view of the segment.

   overriding function To_UTF_8_String (Self : UTF8_Segment) return String;
   --  Returns internal data as standard String.

   type UTF8_Text is
     new Magic.Strings.Reference_Counted.Abstract_Shared_String with null record;

   overriding function Is_Empty (Self : UTF8_Text) return Boolean;
   --  Return True when string is empty.

   overriding function Element
     (Self     : UTF8_Text;
      Position : Cursor) return Magic.Unicode.Code_Point;

   overriding procedure First_Character
     (Self     : UTF8_Text;
      Position : in out Cursor);
   --  Initialize iterator to point to first character of the string

   overriding function Forward
     (Self     : UTF8_Text;
      Position : in out Cursor) return Boolean;
   --  Move cursor one character forward. Return True on success.

   overriding function To_Text (Self : in out UTF8_Text) return String_Access;
   --  Returns itself.

   overriding function To_UTF_8_String (Self : UTF8_Text) return String;
   --  Returns internal data as standard String.

end Magic.Strings.UTF8;
