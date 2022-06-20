--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Stream_Element_Vectors.Internals is

   --------------------------
   -- Data_Constant_Access --
   --------------------------

   procedure Data_Constant_Access
     (Self    : Stream_Element_Vector'Class;
      Length  : out Ada.Streams.Stream_Element_Count;
      Storage : out Stream_Element_Array_Access) is
   begin
      if Self.Data /= null then
         Length  := Self.Data.Length;
         Storage := Self.Data.Storage'Unrestricted_Access;

      else
         Length  := 0;
         Storage := null;
      end if;
   end Data_Constant_Access;

end VSS.Stream_Element_Vectors.Internals;
