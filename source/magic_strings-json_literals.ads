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
--  Implementation of the string which uses slices of JSON text.

with Magic_Strings.UTF8;

private package Magic_Strings.JSON_Literals is

   pragma Preelaborate;

   type JSON_Literal_String (<>) is abstract new Abstract_String with private;
   --  It represents string literals in a JSON document.

private

   type UTF8_Shared_String_Access is
     access all Magic_Strings.UTF8.UTF8_Shared_String;

   type UTF8_Span is record
      First : Magic_Strings.UTF8.Code_Unit_Count;
      --  First points to the begining of the first character.
      Last  : Magic_Strings.UTF8.Code_Unit_Count;
      --  Last points to the end of the last character.
   end record;
   --  Indexes of the UTF8 Code_Unit in a shared JSON text.

   type UTF8_Span_Array is array (Positive range <>) of UTF8_Span;

   type JSON_Literal_String (Last_Escape : Natural) is
     abstract new Abstract_String with
   record
      JSON : UTF8_Shared_String_Access;
      --  Text of JSON is shared between all JSON literals contained in it.

      Span : UTF8_Span;
      --  Span of the literal on JSON (not including quotes

      Escapes : UTF8_Span_Array (1 .. Last_Escape);
      --  List of escape sequences in the literal if any
   end record;

   overriding function Reference
     (Self : in out JSON_Literal_String) return String_Access;

   overriding procedure Unreference (Self : in out JSON_Literal_String);

end Magic_Strings.JSON_Literals;
