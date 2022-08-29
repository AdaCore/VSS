--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Vector of strings and operations on it.

pragma Ada_2022;
--  GNAT: different versions of compiler use different pragmas to enable
--  Ada 2022 features.

with Ada.Iterator_Interfaces;

private with Ada.Finalization;
private with Ada.Streams;

with VSS.Characters;
private with VSS.Implementation.String_Vectors;
with VSS.Strings;

package VSS.String_Vectors is

   pragma Preelaborate;
   pragma Remote_Types;

   type Virtual_String_Vector is tagged private
     with
       Constant_Indexing => Element,
       Default_Iterator  => Iterate,
       Iterator_Element  => VSS.Strings.Virtual_String,
       Aggregate         => (Empty       => Empty_Virtual_String_Vector,
                             Add_Unnamed => Append_Syntax_Sugar);

   Empty_Virtual_String_Vector : constant Virtual_String_Vector;

   overriding function "="
     (Left  : Virtual_String_Vector;
      Right : Virtual_String_Vector) return Boolean;
   --  Compare two string vectors.

   function Is_Empty (Self : Virtual_String_Vector'Class) return Boolean;
   --  Return True when string vector is empty.

   function Length (Self : Virtual_String_Vector'Class) return Natural;
   --  Number of elements in the vector.

   function Element
     (Self  : Virtual_String_Vector'Class;
      Index : Positive) return VSS.Strings.Virtual_String;
   --  Return given element. Return "null" string when index is out of bound.

   function Last_Element
     (Self : Virtual_String_Vector'Class) return VSS.Strings.Virtual_String;
   --  Return last element of the vector.

   procedure Clear (Self : in out Virtual_String_Vector'Class);
   --  Remove all strings from the vector

   procedure Append
     (Self : in out Virtual_String_Vector'Class;
      Item : VSS.Strings.Virtual_String'Class);
   --  Append string to the end of the vector.

   procedure Append
     (Self : in out Virtual_String_Vector'Class;
      Item : Virtual_String_Vector'Class);
   --  Append strings of Item to the end of the vector.

   procedure Prepend
     (Self : in out Virtual_String_Vector'Class;
      Item : VSS.Strings.Virtual_String'Class);
   --  Insert given string before existing strings in the vector.

   procedure Delete
     (Self  : in out Virtual_String_Vector'Class;
      Index : Positive);
   --  Delete element at the given index.

   function Delete_First
     (Self : Virtual_String_Vector'Class) return Virtual_String_Vector;
   --  Delete first element and return result vector.

   procedure Delete_First (Self : in out Virtual_String_Vector'Class);
   --  Delete first element from the vector.

   procedure Delete_Last (Self : in out Virtual_String_Vector'Class);
   --  Delete the last element.

   procedure Replace
     (Self  : in out Virtual_String_Vector'Class;
      Index : Positive;
      Item  : VSS.Strings.Virtual_String'Class)
        with Pre => Index <= Self.Length;
   --  Replace a string vector item with given Index by a new value.

   function Contains
     (Self             : Virtual_String_Vector'Class;
      Item             : VSS.Strings.Virtual_String;
      Case_Sensitivity : VSS.Strings.Case_Sensitivity :=
        VSS.Strings.Case_Sensitive)
      return Boolean;
   --  Return True when vector contains given string.

   function Join
     (Self      : Virtual_String_Vector'Class;
      Separator : VSS.Characters.Virtual_Character)
      return VSS.Strings.Virtual_String;
   --  Join all strings in the string vector into single string with each
   --  element separated by the given separator.

   function Join_Lines
     (Self           : Virtual_String_Vector'Class;
      Terminator     : VSS.Strings.Line_Terminator;
      Terminate_Last : Boolean := True)
      return VSS.Strings.Virtual_String;
   --  Join all string vector's strings with each element separated by given
   --  Terminator. When Terminate_Last is True line terminator is added after
   --  last line.

   --  Syntax sugar for Ada 2012 user-defined iterator

   type Cursor is private;

   function Element
     (Self     : Virtual_String_Vector'Class;
      Position : Cursor) return VSS.Strings.Virtual_String;

   function Has_Element (Self : Cursor) return Boolean
     with Inline;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Cursor, Has_Element);

   type Reversible_Iterator is
     limited new Iterator_Interfaces.Reversible_Iterator with private;

   overriding function First (Self : Reversible_Iterator) return Cursor;

   overriding function Next
     (Self     : Reversible_Iterator;
      Position : Cursor) return Cursor
        with Inline;

   overriding function Last (Self : Reversible_Iterator) return Cursor;

   overriding function Previous
     (Self     : Reversible_Iterator;
      Position : Cursor) return Cursor
        with Inline;

   function Iterate
     (Self : Virtual_String_Vector'Class) return Reversible_Iterator;
   --  Return an iterator over each element in the vector

   --  Syntax sugar for Ada 2022 aggregates

   procedure Append_Syntax_Sugar
     (Self : in out Virtual_String_Vector;
      Item : VSS.Strings.Virtual_String);
   --  Append operation to match expected profile of Aggregate aspect.

private

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Virtual_String_Vector);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Virtual_String_Vector);

   type Virtual_String_Vector is new Ada.Finalization.Controlled with record
      Data : aliased
        VSS.Implementation.String_Vectors.String_Vector_Data_Access;
   end record
     with Read => Read, Write => Write;

   overriding procedure Adjust (Self : in out Virtual_String_Vector);
   overriding procedure Finalize (Self : in out Virtual_String_Vector);

   Empty_Virtual_String_Vector : constant Virtual_String_Vector :=
     (Ada.Finalization.Controlled with others => <>);

   type Reversible_Iterator is
     limited new Iterator_Interfaces.Reversible_Iterator with
   record
      Last : Natural;
   end record;

   type Cursor is record
      Index : Natural;
   end record;

   function Has_Element (Self : Cursor) return Boolean is (Self.Index > 0);

end VSS.String_Vectors;
