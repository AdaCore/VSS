--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Stream_Element_Vectors;
with VSS.Strings;

package VSS.Utils.File_IO is

   function Load
     (Name : String) return VSS.Stream_Element_Vectors.Stream_Element_Vector;
   --  Load content of the file and return it.

   function Load
     (Name     : String;
      Encoding : String) return VSS.Strings.Virtual_String;
   --  Load content of the file, decode it and return result. Raise
   --  Constraint_Error when encoding is not supported or data decoding fails.

end VSS.Utils.File_IO;
