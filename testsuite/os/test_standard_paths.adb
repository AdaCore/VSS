--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Wide_Wide_Text_IO;

with VSS.Standard_Paths;
with VSS.Strings.Conversions;

with Test_Support;

procedure Test_Standard_Paths is
   Home : constant VSS.Strings.Virtual_String :=
     VSS.Standard_Paths.Writable_Location
       (VSS.Standard_Paths.Home_Location);
   Temp : constant VSS.Strings.Virtual_String :=
     VSS.Standard_Paths.Writable_Location
       (VSS.Standard_Paths.Temp_Location);

begin
   Ada.Wide_Wide_Text_IO.Put_Line
     (VSS.Strings.Conversions.To_Wide_Wide_String (Home));
   Ada.Wide_Wide_Text_IO.Put_Line
     (VSS.Strings.Conversions.To_Wide_Wide_String (Temp));

   Test_Support.Assert (not Home.Is_Empty);
   Test_Support.Assert (not Temp.Is_Empty);
end Test_Standard_Paths;
