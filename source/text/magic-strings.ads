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
--  API to process string data as sequences of Unicode Code Points.

private with Ada.Finalization;
private with Ada.Strings.UTF_Encoding;
private with Ada.Streams;
private with System.Storage_Elements;

with Magic.Characters;
limited with Magic.Strings.Iterators.Characters;
limited with Magic.Strings.Texts;
private with Magic.Unicode;

package Magic.Strings is

   pragma Preelaborate;
   pragma Remote_Types;

   type Character_Count is range 0 .. 2 ** 30 - 1;
   subtype Character_Index is Character_Count range 1 .. Character_Count'Last;

   type Grapheme_Count is range 0 .. 2 ** 30 - 1;
   subtype Grapheme_Index is Grapheme_Count range 1 .. Grapheme_Count'Last;

   type Magic_String is tagged private;
   pragma Preelaborable_Initialization (Magic_String);

   Empty_Magic_String : constant Magic_String;

   function Is_Empty (Self : Magic_String'Class) return Boolean;
   --  Return True when string is empty string: it is ether null or has zero
   --  length.

   function Is_Null (Self : Magic_String'Class) return Boolean;
   --  Return True when string is null.

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

   type Abstract_String_Handler is tagged;

   type String_Handler_Access is access all Abstract_String_Handler'Class;

   ------------
   -- Cursor --
   ------------

   type Cursor is record
      Index        : Character_Index                     := 1;
      UTF8_Offset  : Magic.Unicode.UTF8_Code_Unit_Index  := 0;
      UTF16_Offset : Magic.Unicode.UTF16_Code_Unit_Index := 0;
   end record;

   -----------------
   -- String_Data --
   -----------------

   --  String_Data is a pair or Handler and pointer to the associated data.
   --  It is not defined how particular implementation of the String_Handler
   --  use pointer.

   --  XXX Not implemented:
   --  However, there is one exception: when In_Place Flag is set it means
   --  that special predefined handler is used and both Handler and Data
   --  fields are used as storage. By convention, in place data is always
   --  use UTF-8 encoding.

   type String_Data (In_Place : Boolean := False) is record
      Handler : String_Handler_Access;
      Pointer : System.Address;
   end record;

   -----------------------------
   -- Abstract_String_Handler --
   -----------------------------

   --  Abstract_String_Hanlder is abstract set of operations on string data.

   type Abstract_String_Handler is abstract tagged limited null record;

   not overriding procedure Reference
     (Self    : Abstract_String_Handler;
      Pointer : in out System.Address) is abstract;
   --  Called when new copy of the string is created. It should update pointer
   --  if necessary.

   not overriding procedure Unreference
     (Self    : Abstract_String_Handler;
      Pointer : in out System.Address) is abstract;
   --  Called when some copy of the string is not longer needed. It should
   --  release resources when necessary and reset Pointer to safe value.

   not overriding function Is_Empty
     (Self    : Abstract_String_Handler;
      Pointer : System.Address) return Boolean is abstract;
   --  Return True when string is empty.

   not overriding function Element
     (Self     : Abstract_String_Handler;
      Pointer  : System.Address;
      Position : Magic.Strings.Cursor)
      return Magic.Unicode.Code_Point is abstract;
   --  Return character at given position or NUL if Position is not pointing
   --  to any character.

   not overriding procedure First_Character
     (Self     : Abstract_String_Handler;
      Pointer  : System.Address;
      Position : in out Magic.Strings.Cursor) is abstract;
   --  Initialize iterator to point to first character.

   not overriding function Forward
     (Self     : Abstract_String_Handler;
      Pointer  : System.Address;
      Position : in out Cursor) return Boolean is abstract;
   --  Move cursor one character forward. Return True on success.

   not overriding procedure From_UTF_8_String
     (Self    : Abstract_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Pointer : out System.Address;
      Success : out Boolean) is abstract;
   --  Convert UTF_8_String into internal representation.

   not overriding function To_UTF_8_String
     (Self    : Abstract_String_Handler;
      Pointer : System.Address)
      return Ada.Strings.UTF_Encoding.UTF_8_String is abstract;
   --  Converts string data into standard UTF_8_String.

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
      --  Data : String_Access;
      Data : String_Data;
      Head : Referal_Limited_Access;
      Tail : Referal_Limited_Access;
   end record
     with Read  => Read,
          Write => Write;

   overriding procedure Adjust (Self : in out Magic_String);
   overriding procedure Finalize (Self : in out Magic_String);

   Empty_Magic_String : constant Magic_String :=
     (Ada.Finalization.Controlled with
        Data => <>, Head => null, Tail => null);

   -----------------------
   -- Grapheme_Iterator --
   -----------------------

   type Grapheme_Iterator is limited new Referal_Limited_Base with record
      null;
   end record;

   overriding procedure Invalidate (Self : in out Grapheme_Iterator) is null;

   --  use type System.Storage_Elements.Storage_Offset;
   --
   --  type Unsigned_31 is mod 2**31;
   --
   --  type Union_Data (Is_In_Place : Boolean := True) is record
   --     Capacity : Unsigned_31;
   --
   --     Leading  : System.Storage_Elements.Storage_Array
   --       (0 .. System.Address'Max_Size_In_Storage_Elements - 5);
   --
   --     case Is_In_Place is
   --        when False =>
   --           Handler : String_Handler;
   --           Data    : aliased System.Address;
   --
   --        when True =>
   --           Storage : aliased System.Storage_Elements.Storage_Array
   --             (0 .. 2 * System.Address'Max_Size_In_Storage_Elements - 1);
   --     end case;
   --  end record with Pack; --  with Unchecked_Union;
   --
   --  type New_Magic_String is new Ada.Finalization.Controlled with record
   --     --  Capacity    : Unsigned_31;
   --     --  --  Is_In_Place : Boolean;
   --     Data        : Union_Data;
   --     Head : Referal_Limited_Access;
   --     Tail : Referal_Limited_Access;
   --  end record with Pack;

end Magic.Strings;
