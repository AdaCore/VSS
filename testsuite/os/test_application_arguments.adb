--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Application;
with VSS.String_Vectors;
with VSS.Strings;

with Test_Support;

procedure Test_Application_Arguments is

   use type VSS.Strings.Virtual_String;

   Args : constant VSS.String_Vectors.Virtual_String_Vector :=
     VSS.Application.Arguments;

begin
   Test_Support.Assert (Args.Length = 4);
   Test_Support.Assert (Args (1) = "hello");
   Test_Support.Assert (Args (2) = "привет");
   Test_Support.Assert (Args (3) = "გამარჯობა");
   Test_Support.Assert (Args (4) = "👋");
end Test_Application_Arguments;
