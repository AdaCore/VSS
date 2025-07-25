--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with JSON_Schema.Readers;

package JSON_Schema.Writers.Outputs is

   procedure Generate_Writers
     (Map            : JSON_Schema.Readers.Schema_Map;
      Root_Package   : VSS.Strings.Virtual_String;
      Enum_Package   : VSS.Strings.Virtual_String;
      Header         : VSS.String_Vectors.Virtual_String_Vector;
      Holders        : VSS.String_Vectors.Virtual_String_Vector;
      Optional_Types : String_Sets.Set;
      Keep_Extra     : Boolean);
   --  Generate Write routines types for schemas in given Map

end JSON_Schema.Writers.Outputs;
