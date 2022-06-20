--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  @private
--
--  This package is for internal use only.

with Ada.Streams;

package VSS.Stream_Element_Vectors.Internals is

   pragma Preelaborate;

   type Stream_Element_Array_Access is
     access constant Ada.Streams.Stream_Element_Array
                      (1 .. Ada.Streams.Stream_Element_Offset'Last);

   procedure Data_Constant_Access
     (Self    : Stream_Element_Vector'Class;
      Length  : out Ada.Streams.Stream_Element_Count;
      Storage : out Stream_Element_Array_Access);

end VSS.Stream_Element_Vectors.Internals;
