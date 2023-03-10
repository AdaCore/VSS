--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides conversion subprograms between object of
--  Stream_Element_Vector type and some types from the standard
--  run-time library (String, Unbounded_String, Stream_Element_Array).
--
--  Conversion subprograms must be used carefully, some of them returns object
--  of unconstrained type and its use may be dangerous due to possible stack
--  overflow or memory corruption for large amount of elements.

with Ada.Strings.Unbounded;

package VSS.Stream_Element_Vectors.Conversions is

   pragma Preelaborate;

   function Unchecked_To_String
     (Item : Stream_Element_Vector'Class) return String;
   --  Convert content of the buffer to standard string. It do binary
   --  conversion without any assumptions or checks.

   function Unchecked_From_Unbounded_String
     (Item : Ada.Strings.Unbounded.Unbounded_String)
      return Stream_Element_Vector;
   --  Converts Unbounded_String into Stream_Element_Buffer without any checks.

   function To_Stream_Element_Vector
     (Item : Ada.Streams.Stream_Element_Array) return Stream_Element_Vector;
   --  Convert Stream_Element_Array into Stream_Element_Buffer.

   function To_Stream_Element_Array
     (Item : Stream_Element_Vector'Class)
      return Ada.Streams.Stream_Element_Array;
   --  Return data as array of stream elements.
   --
   --  Note: this function returns object of unconstrained type, thus its use
   --  may result in stack overflow or memory corruption.

end VSS.Stream_Element_Vectors.Conversions;
