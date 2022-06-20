--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

procedure Test_String_Hash is

   use type VSS.Strings.Hash_Type;

   N  : VSS.Strings.Virtual_String;
   pragma Warnings (Off, N);
   E  : constant VSS.Strings.Virtual_String := "";
   S1 : constant VSS.Strings.Virtual_String := "foobar";
   S2 : constant VSS.Strings.Virtual_String := "кириллица";

begin
   if N.Hash /= 16#CBF2_9CE4_8422_2325# then
      raise Program_Error;
   end if;

   if N.Hash /= E.Hash then
      raise Program_Error;
   end if;

   if S1.Hash /= 16#6314_4B53_BA2E_7122# then
      raise Program_Error;
   end if;

   if S2.Hash /= 16#0FB0_AE54_FCE4_751D# then
      raise Program_Error;
   end if;
end Test_String_Hash;
