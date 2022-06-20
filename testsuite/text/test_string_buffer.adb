--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Characters;
with VSS.Strings;

procedure Test_String_Buffer is

   use type VSS.Strings.Virtual_String;

begin
   --  Few append operations to null string.

   declare
      Buffer : VSS.Strings.Virtual_String;

   begin
      Buffer.Append ('A');
      Buffer.Append ('Б');
      Buffer.Append ('क');
      Buffer.Append ('𐌈');

      if Buffer /= VSS.Strings.To_Virtual_String ("AБक𐌈") then
         raise Program_Error;
      end if;
   end;

   --  Few append operations to small initial string, enough to overlow
   --  "in place" data buffer.

   declare
      Buffer : VSS.Strings.Virtual_String :=
        VSS.Strings.To_Virtual_String ("********");

   begin
      Buffer.Append ('A');
      Buffer.Append ('Б');
      Buffer.Append ('क');
      Buffer.Append ('𐌈');

      if Buffer /= VSS.Strings.To_Virtual_String ("********AБक𐌈") then
         raise Program_Error;
      end if;
   end;
end Test_String_Buffer;
