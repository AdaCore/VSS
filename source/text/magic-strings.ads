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
--  API to process string data as sequences of Unicode Code Points.

private with Ada.Finalization;
private with Ada.Strings.UTF_Encoding;
private with Ada.Streams;

with Magic.Characters;
limited with Magic.Strings.Iterators.Characters;
limited with Magic.Strings.Texts;
private with Magic.Unicode;

package Magic.Strings is

   pragma Preelaborate;
   pragma Remote_Types;

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
     (Self : Magic_String) return Magic.Strings.Texts.Magic_Text;

   type Grapheme_Iterator is tagged limited private;

   function First_Character
     (Self : Magic_String'Class)
      return Magic.Strings.Iterators.Characters.Character_Iterator;

   --  function Last_Character
   --    (Self : Magic_String'Class) return Character_Iterator;
   --
   --  function First_Grapheme
   --    (Self : Magic_String'Class) return Grapheme_Iterator;
   --
   --  function Last_Grapheme
   --    (Self : Magic_String'Class) return Grapheme_Iterator;

private

   type Magic_String_Access is access all Magic_String'Class;

   ------------
   -- Cursor --
   ------------

   type Cursor is record
      Index        : Character_Index                     := 1;
      UTF8_Offset  : Magic.Unicode.UTF8_Code_Unit_Index  := 0;
      UTF16_Offset : Magic.Unicode.UTF16_Code_Unit_Index := 0;
   end record;

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

--  function Length (Self : Abstract_String) return Character_Count is abstract;
   --  Return number of abstract characters in the string.

   procedure First_Character
     (Self     : Abstract_String;
      Position : in out Cursor) is abstract;
   --  Initialize iterator to point to first character.

   function Forward
     (Self     : Abstract_String;
      Position : in out Cursor) return Boolean is abstract;
   --  Move cursor one character forward. Return True on success.

   function Element
     (Self     : Abstract_String;
      Position : Cursor) return Magic.Unicode.Code_Point is abstract;
   --  Return character at given position or NUL is position is not pointing
   --  to any character.

   function To_UTF_8_String
     (Self : Abstract_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String is abstract;
   --  Converts string data into standard UTF_8_String.

   function To_Text
     (Self : in out Abstract_String) return String_Access is abstract;
   --  Returns view that supports text operations. Returned value must be
   --  unreferenced after use.

   --------------------------
   -- Referal_Limited_Base --
   --------------------------

   type Referal_Limited_Base is tagged;

   type Referal_Limited_Access is access all Referal_Limited_Base'Class;

   type Referal_Limited_Base is
     abstract new Ada.Finalization.Limited_Controlled with record
      Owner    : Magic_String_Access;
      Next     : Referal_Limited_Access;
      Previous : Referal_Limited_Access;
   end record;

   procedure Connect
     (Self  : in out Referal_Limited_Base'Class;
      Owner : not null Magic_String_Access);
   --  Connect referal to string object

   procedure Disconnect (Self  : in out Referal_Limited_Base'Class);
   --  Disconnect referel from string object

   procedure Invalidate (Self : in out Referal_Limited_Base) is abstract;

   overriding procedure Finalize (Self : in out Referal_Limited_Base);
   --  Invalidate referal state and disconnect from the string object.

   ------------------
   -- Magic_String --
   ------------------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Magic_String);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Magic_String);

   type Magic_String is new Ada.Finalization.Controlled with record
      Data : String_Access;
      Head : Referal_Limited_Access;
      Tail : Referal_Limited_Access;
   end record
     with Read  => Read,
          Write => Write;

   overriding procedure Adjust (Self : in out Magic_String);
   overriding procedure Finalize (Self : in out Magic_String);

   Empty_Magic_String : constant Magic_String :=
     (Ada.Finalization.Controlled with
        Data => null, Head => null, Tail => null);

   -----------------------
   -- Grapheme_Iterator --
   -----------------------

   type Grapheme_Iterator is limited new Referal_Limited_Base with record
      null;
   end record;

   overriding procedure Invalidate (Self : in out Grapheme_Iterator) is null;

end Magic.Strings;
