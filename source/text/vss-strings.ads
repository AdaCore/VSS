------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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
private with Ada.Streams;

with VSS.Characters;
private with VSS.Implementation.Strings;
limited with VSS.String_Vectors;
limited with VSS.Strings.Cursors.Iterators.Characters;
limited with VSS.Strings.Cursors.Iterators.Lines;
limited with VSS.Strings.Texts;

package VSS.Strings is

   pragma Preelaborate;
   pragma Remote_Types;

   type Character_Count is range 0 .. 2 ** 30 - 1;
   subtype Character_Index is Character_Count range 1 .. Character_Count'Last;

   type Grapheme_Count is range 0 .. 2 ** 30 - 1;
   subtype Grapheme_Index is Grapheme_Count range 1 .. Grapheme_Count'Last;

   type Hash_Type is mod 2**64;

   type Line_Terminator is (CR, LF, CRLF, NEL, VT, FF, LS, PS);

   type Line_Terminator_Set is array (Line_Terminator) of Boolean
     with Pack, Default_Component_Value => False;

   New_Line_Function : constant Line_Terminator_Set :=
     (CR | LF | CRLF | NEL => True, others => False);

   type Virtual_String is tagged private
     with String_Literal => To_Virtual_String;
   pragma Preelaborable_Initialization (Virtual_String);

   Empty_Virtual_String : constant Virtual_String;

   function Is_Empty (Self : Virtual_String'Class) return Boolean;
   --  Return True when string is empty string: it is ether null or has zero
   --  length.

   function Is_Null (Self : Virtual_String'Class) return Boolean;
   --  Return True when string is null.

   function Hash (Self : Virtual_String'Class) return Hash_Type;
   --  Return hash value for the given string.

   function Character_Length
     (Self : Virtual_String'Class) return Character_Count;
   --  Return number of characters.

   function To_Magic_Text
     (Self : Virtual_String) return VSS.Strings.Texts.Magic_Text;

   type Grapheme_Iterator is tagged limited private;

   function First_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator;
   --  Return iterator pointing to the first character of the stirng.

   function Last_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator;
   --  Return iterator pointing to the last character of the stirng.

   function Character
     (Self     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator;
   --  Return iterator pointing to the character at given position. Cursor
   --  must belong to the same string.

   --  function First_Grapheme
   --    (Self : Magic_String'Class) return Grapheme_Iterator;
   --
   --  function Last_Grapheme
   --    (Self : Magic_String'Class) return Grapheme_Iterator;

   function First_Line
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator;
   --  Return iterator pointing to the first logical line of the string.

   function Line
     (Self            : Virtual_String'Class;
      Position        : VSS.Strings.Cursors.Abstract_Character_Cursor'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator;
   --  Return iterator pointing to the line at given position.

   --  function Last_Line
   --    (Self            : Virtual_String'Class;
   --     Terminators     : Line_Terminator_Set := New_Line_Function;
   --     Keep_Terminator : Boolean := False)
   --     return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator;

   overriding function "="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean;
   function "<"
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean;
   function "<="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean;
   function ">"
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean;
   function ">="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean;
   --  Compare two strings in binary order of code points.

   --  function "&"
   --    (Left  : Virtual_String;
   --     Right : Virtual_String) return Virtual_String;
   --  function "&"
   --    (Left  : Virtual_String;
   --     Right : VSS.Characters.Virtual_Character) return Virtual_String;
   --  function "&"
   --    (Left  : VSS.Characters.Virtual_Character;
   --     Right : Virtual_String) return Virtual_String;
   --  function "&"
   --    (Left  : VSS.Characters.Virtual_Character;
   --     Right : VSS.Characters.Virtual_Character) return Virtual_String;
   --  --  Concatenation operator for virtual string and virtual characters.

   procedure Clear (Self : in out Virtual_String'Class);
   --  Remove all data.

   procedure Append
     (Self : in out Virtual_String'Class;
      Item : VSS.Characters.Virtual_Character);
   --  Append given abstract character to the end of the string.

   procedure Append
     (Self : in out Virtual_String'Class;
      Item : Virtual_String'Class);
   --  Append another string to the end of the string.

   --  function Append
   --    (Self : Virtual_String'Class;
   --     Item : VSS.Characters.Virtual_Character) return Virtual_String;
   --  --  Append given abstract character to the end of the string and returns
   --  --  result.
   --
   --  procedure Append
   --    (Self : in out Virtual_String'Class;
   --     Item : Virtual_String'Class);
   --  --  Append given string to the end of the string.
   --
   --  function Append
   --    (Self : Virtual_String'Class;
   --     Item : Virtual_String'Class) return Virtual_String;
   --  --  Append given string to the end of the string and returns result.

   --  procedure Prepend
   --    (Self : in out Virtual_String'Class;
   --     Item : VSS.Characters.Virtual_Character);
   --  --  Prepend given character to the begin of the string.
   --
   --  procedure Prepend
   --    (Self : in out Virtual_String'Class;
   --     Item : Virtual_String'Class);
   --  --  Prepend given string to the begin of the string.

   procedure Insert
     (Self     : in out Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Item     : VSS.Characters.Virtual_Character);
   --  procedure Insert
   --    (Self     : in out Virtual_String'Class;
   --     Position : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     Item     : Virtual_String'Class);
   --  --  Inserts given item at the given position. Do nothing if the given
   --  --  position is invalid.

   --  function Insert
   --    (Self     : Virtual_String'Class;
   --     Position : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     Item     : VSS.Characters.Virtual_Character) return Virtual_String;
   --  function Insert
   --    (Self     : Virtual_String'Class;
   --     Position : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     Item     : Virtual_String'Class) return Virtual_String;
   --  --  Inserts given item at the given position and returns result. Returns
   --  --  source string if the given position is invalid.

   --  procedure Remove
   --    (Self : in out Virtual_String'Class;
   --     From : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     To   : VSS.Strings.Cursors.Abstract_Cursor'Class);
   --  procedure Remove
   --    (Self    : in out Virtual_String'Class;
   --     From_To : VSS.Strings.Cursors.Abstract_Cursor'Class);
   --  --  Removes characters from of the string starting from given position
   --  --  to given position.

   --  function Remove
   --    (Self : Virtual_String'Class;
   --     From : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     To   : VSS.Strings.Cursors.Abstract_Cursor'Class)
   --     return Virtual_String;
   --  function Remove
   --    (Self    : Virtual_String'Class;
   --     From_To : VSS.Strings.Cursors.Abstract_Cursor'Class)
   --     return Virtual_String;
   --  --  Removes characters from of the string starting from given position
   --  --  to given position and returns result.

   --  procedure Replace
   --    (Self : in out Virtual_String'Class;
   --     From : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     To   : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By   : VSS.Characters.Virtual_Character);
   --  procedure Replace
   --    (Self    : in out Virtual_String'Class;
   --     From_To : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By      : VSS.Characters.Virtual_Character);
   --  procedure Replace
   --    (Self : in out Virtual_String'Class;
   --     From : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     To   : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By   : Virtual_String'Class);
   --  procedure Replace
   --    (Self    : in out Virtual_String'Class;
   --     From_To : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By      : Virtual_String'Class);
   --  --  Replace slice from and to given positions by given item.

   --  function Replace
   --    (Self : Virtual_String'Class;
   --     From : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     To   : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By   : VSS.Characters.Virtual_Character) return Virtual_String;
   --  function Replace
   --    (Self    : Virtual_String'Class;
   --     From_To : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By      : VSS.Characters.Virtual_Character) return Virtual_String;
   --  function Replace
   --    (Self : Virtual_String'Class;
   --     From : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     To   : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By   : Virtual_String'Class) return Virtual_String;
   --  function Replace
   --    (Self    : Virtual_String'Class;
   --     From_To : VSS.Strings.Cursors.Abstract_Cursor'Class;
   --     By      : Virtual_String'Class) return Virtual_String;
   --  --  Replace slice from and to given positions by given item and returns
   --  --  result.

   function Slice
     (Self : Virtual_String'Class;
      From : VSS.Strings.Cursors.Abstract_Cursor'Class;
      To   : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return Virtual_String;
   function Slice
     (Self    : Virtual_String'Class;
      Segment : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return Virtual_String;
   --  Returns slice of the string. Return "null" string when one of cursors
   --  doesn't belog to given string or invalid cursors.

   function Starts_With
     (Self   : Virtual_String'Class;
      Prefix : Virtual_String'Class) return Boolean;
   --  Return True when Self starts with Prefix.
   function Ends_With
     (Self   : Virtual_String'Class;
      Suffix : Virtual_String'Class) return Boolean;
   --  Return True when Self has given Suffix.

   function To_Virtual_String (Item : Wide_Wide_String) return Virtual_String;
   --  Convert given string into virtual string.

   function Split_Lines
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.String_Vectors.Virtual_String_Vector;

private

   type Magic_String_Access is access all Virtual_String'Class;

   ------------------
   -- Referal_Base --
   ------------------

   type Referal_Base is tagged;

   type Referal_Access is access all Referal_Base'Class;

   type Referal_Base is abstract new Ada.Finalization.Controlled with record
      Owner    : Magic_String_Access;
      Next     : Referal_Access;
      Previous : Referal_Access;
   end record;

   procedure Connect
     (Self  : in out Referal_Base'Class;
      Owner : not null Magic_String_Access);
   --  Connect referal to string object

   procedure Disconnect (Self  : in out Referal_Base'Class);
   --  Disconnect referel from string object

   procedure Invalidate (Self : in out Referal_Base) is abstract;

   procedure String_Modified
     (Self     : in out Referal_Base;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is abstract;
   --  Called when referenced string object has been modified. Start is the
   --  position of the first character of the operation, it is state before
   --  modification operation, thus negative UTF* offset is not valid.
   --  Removed and Inserted parameters are sizes of the removed and inserted
   --  segments. All their members are valid.

   overriding procedure Adjust (Self : in out Referal_Base);
   --  Connect new object to the string object.

   overriding procedure Finalize (Self : in out Referal_Base);
   --  Invalidate referal state and disconnect from the string object.

   --------------------------
   -- Referal_Limited_Base --
   --------------------------

   type Referal_Limited_Base is tagged;

   type Referal_Limited_Access is access all Referal_Limited_Base'Class;

   type Referal_Limited_Base is
     abstract limited new Ada.Finalization.Limited_Controlled with record
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

   procedure String_Modified
     (Self     : in out Referal_Limited_Base;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is abstract;
   --  Called when referenced string object has been modified. Start is the
   --  position of the first character of the operation, it is state before
   --  modification operation, thus negative UTF* offset is not valid.
   --  Removed and Inserted parameters are sizes of the removed and inserted
   --  segments. All their members are valid.

   overriding procedure Finalize (Self : in out Referal_Limited_Base);
   --  Invalidate referal state and disconnect from the string object.

   ------------------
   -- Magic_String --
   ------------------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Virtual_String);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Virtual_String);

   type Virtual_String is new Ada.Finalization.Controlled with record
      Limited_Head : Referal_Limited_Access;
      Limited_Tail : Referal_Limited_Access;
      Head         : Referal_Access;
      Tail         : Referal_Access;
      Data         : aliased VSS.Implementation.Strings.String_Data;
   end record
     with Read  => Read,
          Write => Write;

   overriding procedure Adjust (Self : in out Virtual_String);
   overriding procedure Finalize (Self : in out Virtual_String);

   Empty_Virtual_String : constant Virtual_String :=
     (Ada.Finalization.Controlled with
        Data         => <>,
        Head         => null,
        Tail         => null,
        Limited_Head => null,
        Limited_Tail => null);

   -----------------------
   -- Grapheme_Iterator --
   -----------------------

   type Grapheme_Iterator is limited new Referal_Limited_Base with record
      null;
   end record;

   overriding procedure Invalidate (Self : in out Grapheme_Iterator) is null;

   overriding procedure String_Modified
     (Self     : in out Grapheme_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is null;

   function Handler
     (Self : Virtual_String'Class)
      return VSS.Implementation.Strings.String_Handler_Access;
   --  Returns string data handler should be used to process data of given
   --  object.

end VSS.Strings;
