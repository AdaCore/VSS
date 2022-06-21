--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.String_Vectors;

package VSS.String_Vectors.Internals is

   pragma Preelaborate;

   function Data_Access
     (Self : in out VSS.String_Vectors.Virtual_String_Vector)
      return access
        VSS.Implementation.String_Vectors.String_Vector_Data_Access;
   --  Return access to Data member of the given object

end VSS.String_Vectors.Internals;
