--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Atomic_Counters;

with VSS.Implementation.Strings;
with VSS.Strings;

package VSS.Implementation.String_Vectors is

   pragma Preelaborate;

   type String_Data_Array is
     array (Positive range <>) of VSS.Implementation.Strings.String_Data;

   type String_Vector_Data (Bulk : Natural) is record
      Counter : System.Atomic_Counters.Atomic_Counter;
      Last    : Natural := 0;
      Data    : String_Data_Array (1 .. Bulk);
   end record;

   type String_Vector_Data_Access is access all String_Vector_Data;

   procedure Reference (Self : String_Vector_Data_Access) with Inline;

   procedure Unreference (Self : in out String_Vector_Data_Access);

   procedure Append
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data);
   --  Appends "copy" of the given string to the end of the string vector.

   procedure Append_And_Move_Ownership
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data);
   --  Appends given string to the end of the string vector with moving of the
   --  ownership of the string to the string vector.

   procedure Prepend
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data);
   --  Prepend "copy" of the given string to the end of the string vector.

   procedure Delete
     (Self  : in out not null String_Vector_Data_Access;
      Index : Positive);
   --  Delete element at the given index.

   procedure Replace
     (Self  : in out not null String_Vector_Data_Access;
      Index : Positive;
      Item  : VSS.Implementation.Strings.String_Data);
   --  Replace a vector item with a given string.

   procedure Join_Lines
     (Self           : String_Vector_Data_Access;
      Result         : in out VSS.Implementation.Strings.String_Data;
      Terminator     : VSS.Strings.Line_Terminator;
      Terminate_Last : Boolean);
   --  Join string vector's strings using given Terminator.

   function Contains
     (Self : String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) return Boolean;
   --  Return True when given string is present in vector.

end VSS.Implementation.String_Vectors;
