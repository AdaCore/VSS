--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.String_Vectors.Internals is

   -----------------
   -- Data_Access --
   -----------------

   function Data_Access
     (Self : in out VSS.String_Vectors.Virtual_String_Vector)
      return access
        VSS.Implementation.String_Vectors.String_Vector_Data_Access is
   begin
      return Self.Data'Unchecked_Access;
   end Data_Access;

end VSS.String_Vectors.Internals;
