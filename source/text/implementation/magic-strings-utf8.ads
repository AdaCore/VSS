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
with System.Atomic_Counters;

private package Magic.Strings.UTF8 is

   pragma Preelaborate;

   type UTF8_Code_Unit_Array is
     array (Magic.Unicode.UTF8_Code_Unit_Count range <>)
     of Magic.Unicode.UTF8_Code_Unit;

   type UTF8_String_Data
     (Capacity : Magic.Unicode.UTF8_Code_Unit_Count) is
   record
      Counter : System.Atomic_Counters.Atomic_Counter;

      Storage : UTF8_Code_Unit_Array (0 .. Capacity);
      --  Buffer to store string's data. First unused code unit is set to
      --  zero, to allow to pass data to C.

      Size    : Magic.Unicode.UTF8_Code_Unit_Count;
      --  Number of code units in the buffer.

      Length  : Character_Count;
      --  Length of the string in Unicode Code Points.
   end record;

   type UTF8_String_Data_Access is access all UTF8_String_Data;

   type UTF8_String_Handler is
     new Magic.Strings.Abstract_String_Handler with null record;

   overriding procedure Reference
     (Self    : UTF8_String_Handler;
      Pointer : in out System.Address);
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   overriding procedure Unreference
     (Self    : UTF8_String_Handler;
      Pointer : in out System.Address);
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

   overriding function Is_Empty
     (Self    : UTF8_String_Handler;
      Pointer : System.Address) return Boolean;

   overriding function Element
     (Self     : UTF8_String_Handler;
      Pointer  : System.Address;
      Position : Magic.Strings.Cursor)
      return Magic.Unicode.Code_Point;
   --  Return character at given position or NUL if Position is not pointing
   --  to any character.

   overriding procedure First_Character
     (Self     : UTF8_String_Handler;
      Pointer  : System.Address;
      Position : in out Magic.Strings.Cursor);
   --  Initialize iterator to point to first character.

   overriding function Forward
     (Self     : UTF8_String_Handler;
      Pointer  : System.Address;
      Position : in out Cursor) return Boolean;
   --  Move cursor one character forward. Return True on success.

   overriding procedure From_UTF_8_String
     (Self    : UTF8_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Pointer : out System.Address;
      Success : out Boolean);
   --  Convert UTF_8_String into internal representation.

   overriding function To_UTF_8_String
     (Self    : UTF8_String_Handler;
      Pointer : System.Address)
      return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Converts string data into standard UTF_8_String.

end Magic.Strings.UTF8;
