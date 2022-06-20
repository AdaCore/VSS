--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with JSON_Schema.Readers;

package JSON_Schema.Writers.Types is

   procedure Write
     (Map          : JSON_Schema.Readers.Schema_Map;
      Root_Package : VSS.Strings.Virtual_String;
      Enum_Package : VSS.Strings.Virtual_String;
      Header       : VSS.String_Vectors.Virtual_String_Vector;
      Holders      : VSS.String_Vectors.Virtual_String_Vector);
   --  Generate types for schemas of given Map. Put types into Root_Package
   --  except enumeration types if Enum_Package provided.
   --  Prepend compilation units with given Header.

end JSON_Schema.Writers.Types;
