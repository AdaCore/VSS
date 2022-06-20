--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.String_Vectors;
with VSS.Strings;

generic
   with procedure Do_Test
     (String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector);

procedure Generic_UCD_Break_Test_Runner (File_Name : String);
