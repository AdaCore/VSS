------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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
--  Abstract_String_Hanlder is abstract set of operations on string data.
--  It provides default generic implementation of some operations which
--  derived handlers may override to provide better implementation.

with Ada.Strings.UTF_Encoding;

with VSS.Implementation.FNV_Hash;
with VSS.Implementation.Strings;
with VSS.Implementation.String_Vectors;
limited with VSS.Strings;
with VSS.Unicode;

package VSS.Implementation.String_Handlers is

   pragma Preelaborate;

   use type VSS.Implementation.Strings.Character_Count;

   -----------------------------
   -- Abstract_String_Handler --
   -----------------------------

   type Abstract_String_Handler is abstract tagged limited null record;

   not overriding procedure Reference
     (Self : Abstract_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is abstract;
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   not overriding procedure Unreference
     (Self : Abstract_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is abstract;
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

   not overriding function Is_Empty
     (Self : Abstract_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Boolean is abstract;
   --  Return True when string is empty.

   not overriding procedure Hash
     (Self      : Abstract_String_Handler;
      Data      : VSS.Implementation.Strings.String_Data;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator);
   --  Compute hash value of the string as little-endian UTF-32 encoded
   --  character sequence.

   not overriding function Length
     (Self : Abstract_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return VSS.Implementation.Strings.Character_Count is abstract;
   --  Return number of characters in the text

   not overriding function Element
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point is abstract;
   --  Return character at given position or NUL if Position is not pointing
   --  to any character.

   not overriding function Has_Character
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is abstract;
   --  Return True when position points to the character.

   not overriding procedure Before_First_Character
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) is abstract;
   --  Initialize iterator to point to first character.
   not overriding procedure After_Last_Character
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) is abstract;
   --  Initialize iterator to point to the last character.
   --  This procedure sets Position.UTF16_Offset to UTF16_Code_Unit_Index'Last
   --  for now.

   not overriding function Forward
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
      return Boolean is abstract;
   --  Move cursor one character forward. Return True on success.
   not overriding function Backward
     (Self     : Abstract_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
      return Boolean is abstract;
   --  Move cursor one character backward. Return True on success.

   not overriding function Is_Equal
     (Self       : Abstract_String_Handler;
      Data       : VSS.Implementation.Strings.String_Data;
      Other      : Abstract_String_Handler'Class;
      Other_Data : VSS.Implementation.Strings.String_Data) return Boolean;
   not overriding function Is_Less
     (Self       : Abstract_String_Handler;
      Data       : VSS.Implementation.Strings.String_Data;
      Other      : Abstract_String_Handler'Class;
      Other_Data : VSS.Implementation.Strings.String_Data) return Boolean;
   not overriding function Is_Less_Or_Equal
     (Self       : Abstract_String_Handler;
      Data       : VSS.Implementation.Strings.String_Data;
      Other      : Abstract_String_Handler'Class;
      Other_Data : VSS.Implementation.Strings.String_Data) return Boolean;
   --  Compare two strings for binary equivalence/less/greater of code point
   --  sequences. These subprograms provides generic implementation and can
   --  work with any string handlers in cost of performance. Derived types may
   --  provide better implementation for particular case, but always should
   --  fallback to this implementation.

   not overriding function Starts_With
     (Self           : Abstract_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Prefix_Handler : Abstract_String_Handler'Class;
      Prefix_Data    : VSS.Implementation.Strings.String_Data) return Boolean
     with Pre'Class =>
       Abstract_String_Handler'Class (Self).Length (Data)
         >= Prefix_Handler.Length (Prefix_Data);
   --  Return True when string starts with given prefix. This subprogram
   --  provides generic implementation and can work with any string handlers
   --  in cost of performance.
   not overriding function Ends_With
     (Self           : Abstract_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Suffix_Handler : Abstract_String_Handler'Class;
      Suffix_Data    : VSS.Implementation.Strings.String_Data) return Boolean
     with Pre'Class =>
       Abstract_String_Handler'Class (Self).Length (Data)
         >= Suffix_Handler.Length (Suffix_Data);
   --  Return True when string ends with given suffix. This subprogram
   --  provides generic implementation and can work with any string handlers
   --  in cost of performance.

   not overriding procedure From_Wide_Wide_String
     (Self    : in out Abstract_String_Handler;
      Item    : Wide_Wide_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean) is abstract;
   --  Convert Wide_Wide_String into internal representation.

   not overriding procedure From_UTF_8_String
     (Self    : in out Abstract_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean) is abstract;
   --  Convert UTF_8_String into internal representation.

   not overriding function To_UTF_8_String
     (Self : Abstract_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Ada.Strings.UTF_Encoding.UTF_8_String is abstract;
   --  Converts string data into standard UTF_8_String.

   not overriding procedure Append
     (Self : Abstract_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      Code : VSS.Unicode.Code_Point) is abstract
     with Pre'Class => Code not in 16#D800# .. 16#DFFF#;
   --  Append single code point to the data.

   not overriding procedure Append
     (Self           : Abstract_String_Handler;
      Data           : in out VSS.Implementation.Strings.String_Data;
      Suffix_Handler : Abstract_String_Handler'Class;
      Suffix_Data    : VSS.Implementation.Strings.String_Data);
   --  Append suffix string to the data.
   --  The default implementatio append string in a character by character way.

   not overriding procedure Split_Lines
     (Self            : Abstract_String_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
      is abstract;
   --  Splits string into lines using given set of allowed new line
   --  terminators. Line terminator (character or combination of characters)
   --  are removed unless Keep_Terminator is set to True.

end VSS.Implementation.String_Handlers;
