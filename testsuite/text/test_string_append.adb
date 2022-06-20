--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

procedure Test_String_Append is
   use type VSS.Strings.Virtual_String;

   Long_Literal_1 : constant Wide_Wide_String :=
     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
   Long_Literal_2 : constant Wide_Wide_String :=
     "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";

   Short_1 : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String ("A");
   Short_2 : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String ("B");

   Long_1  : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String
     (Long_Literal_1);

   Long_2  : VSS.Strings.Virtual_String := VSS.Strings.To_Virtual_String
     (Long_Literal_2);
   Copy_1  : constant VSS.Strings.Virtual_String := Short_1;
   Copy_2  : constant VSS.Strings.Virtual_String := Long_2;
begin
   Short_1.Append (Short_2);
   if Short_1 /= VSS.Strings.To_Virtual_String ("AB") then
      raise Program_Error;
   end if;

   Short_2.Append (Long_1);
   if Short_2 /= VSS.Strings.To_Virtual_String ("B" & Long_Literal_1) then
      raise Program_Error;
   end if;

   Long_1.Append (Short_1);
   if Long_1 /= VSS.Strings.To_Virtual_String (Long_Literal_1 & "AB") then
      raise Program_Error;
   end if;

   Long_2.Append (Long_1);
   if Long_2 /= VSS.Strings.To_Virtual_String
     (Long_Literal_2 & Long_Literal_1 & "AB")
   then
      raise Program_Error;
   end if;

   if Copy_1 /= VSS.Strings.To_Virtual_String ("A") then
      raise Program_Error;
   elsif Copy_2 /= VSS.Strings.To_Virtual_String (Long_Literal_2) then
      raise Program_Error;
   end if;

end Test_String_Append;
