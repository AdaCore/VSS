--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Application;
with VSS.Environments;

with VSS.String_Vectors;
with VSS.Strings;

with Test_Support;

procedure Test_Environment is
   use type VSS.Strings.Virtual_String;

begin
   --  Undefined environment variable

   declare
      V0 : constant VSS.Strings.Virtual_String :=
        VSS.Application.System_Environment.Value ("VSS_ENV0");

   begin
      Test_Support.Assert (V0.Is_Null);
   end;

   --  Defined environment variable, value contains platform specific
   --  path separator thus can't be checked.

   declare
      V1 : constant VSS.Strings.Virtual_String :=
        VSS.Application.System_Environment.Value ("VSS_ENV1");

   begin
      Test_Support.Assert (not V1.Is_Null);
      Test_Support.Assert (not V1.Is_Empty);
   end;

   --  Defined environment variable as list of paths.

   declare
      VP1 : constant VSS.String_Vectors.Virtual_String_Vector :=
        VSS.Application.System_Environment.Value_Paths ("VSS_ENV1");

   begin
      Test_Support.Assert (VP1.Length = 3);
      Test_Support.Assert (VP1 (1) = "A");
      Test_Support.Assert (VP1 (2) = "B");
      Test_Support.Assert (VP1 (3) = "C");
   end;
end Test_Environment;
