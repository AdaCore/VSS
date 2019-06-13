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
--  API to process string data as sequences of Unicode Code Points.

private with Ada.Finalization;
private with Ada.Strings.UTF_Encoding;

limited with Magic_Strings.Texts;

package Magic_Strings is

   pragma Preelaborate;
--   pragma Remote_Types;

   type Character_Count is new Natural;
   subtype Character_Index is Character_Count range 1 .. Character_Count'Last;

   type Grapheme_Count is new Natural;
   subtype Grapheme_Index is Grapheme_Count range 1 .. Grapheme_Count'Last;

   type Magic_String is tagged private;
   pragma Preelaborable_Initialization (Magic_String);

   Empty_Magic_String : constant Magic_String;

   function Is_Empty (Self : Magic_String'Class) return Boolean;
   --  Return True when string is empty string.

   function To_Magic_Text
     (Self : Magic_String) return Magic_Strings.Texts.Magic_Text;

private

   ---------------------
   -- Abstract_String --
   ---------------------

   --  Abstract_String is an internal representation of the data and common
   --  set of operations to process data. It is designed to allow to use
   --  impicit data sharing (also known as copy-on-write), while
   --  implementations may avoid this if necessary.

   type Abstract_String is abstract tagged limited null record;

   type String_Access is access all Abstract_String'Class;

   function Reference
     (Self : in out Abstract_String) return String_Access is abstract;
   --  Called when new copy of the string is created. It can return parameter
   --  or new allocted data object.

   procedure Unreference (Self : in out Abstract_String) is abstract;
   --  Called when some copy of the string is not longer in use. It should
   --  deallocate data when necessary.

   function Is_Empty (Self : Abstract_String) return Boolean is abstract;
   --  Return True when string is empty.

   function To_UTF_8_String
     (Self : Abstract_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String is abstract;
   --  Converts string data into standard UTF_8_String.

   function To_Text
     (Self : in out Abstract_String) return String_Access is abstract;
   --  Returns view that supports text operations. Returned value must be
   --  unreferenced after use.

   ------------------
   -- Magic_String --
   ------------------

   type Magic_String is new Ada.Finalization.Controlled with record
      Data : String_Access;
   end record;

   overriding procedure Adjust (Self : in out Magic_String);
   overriding procedure Finalize (Self : in out Magic_String);

   Empty_Magic_String : constant Magic_String :=
     (Ada.Finalization.Controlled with Data => null);

end Magic_Strings;
