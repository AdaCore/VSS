--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

end VSS.Stream_Element_Vectors.Conversions;
