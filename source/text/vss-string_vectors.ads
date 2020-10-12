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
--  Vector of strings and operations on it.

with Ada.Iterator_Interfaces;

private with Ada.Finalization;
private with Ada.Streams;

private with VSS.Implementation.String_Vectors;
with VSS.Strings;

package VSS.String_Vectors is

   pragma Preelaborate;
   pragma Remote_Types;

   type Virtual_String_Vector is tagged private
     with
       Constant_Indexing => Element,
       Default_Iterator  => Iterate,
       Iterator_Element  => VSS.Strings.Virtual_String;

   function Length (Self : Virtual_String_Vector'Class) return Natural;
   --  Number of elements in the vector.

   function Element
     (Self  : Virtual_String_Vector'Class;
      Index : Positive) return VSS.Strings.Virtual_String;
   --  Return given element. Return "null" string when index is out of bound.

   procedure Append
     (Self : in out Virtual_String_Vector'Class;
      Item : VSS.Strings.Virtual_String'Class);
   --  Append string to the end of the vector.

   --  Syntax sugar for Ada 2012 user-defined iterator

   function Has_Element (Index : Natural) return Boolean
     with Inline;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Natural, Has_Element);

   type Reversible_Iterator is
     limited new Iterator_Interfaces.Reversible_Iterator with private;

   overriding function First (Self : Reversible_Iterator) return Natural;

   overriding function Next
     (Self     : Reversible_Iterator;
      Position : Natural) return Natural;

   overriding function Last (Self : Reversible_Iterator) return Natural;

   overriding function Previous
     (Self     : Reversible_Iterator;
      Position : Natural) return Natural;

   function Iterate
     (Self : Virtual_String_Vector'Class) return Reversible_Iterator;
   --  Return an interator over each element in the vector

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

   type Reversible_Iterator is
     limited new Iterator_Interfaces.Reversible_Iterator with
   record
      Last : Natural;
   end record;

   function Has_Element (Index : Natural) return Boolean is (Index > 0);

end VSS.String_Vectors;
