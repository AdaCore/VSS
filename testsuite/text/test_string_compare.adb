--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Conversions;

procedure Test_String_Compare is

   use type VSS.Strings.Virtual_String;

   --  "ASCII Кириллица ⊗∬ 𝛻𝜕 "
   S1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String
       ((Character'Val (16#41#),
        Character'Val (16#53#),
        Character'Val (16#43#),
        Character'Val (16#49#),
        Character'Val (16#49#),
        Character'Val (16#20#),
        Character'Val (16#D0#),
        Character'Val (16#9A#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#80#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#86#),
        Character'Val (16#D0#),
        Character'Val (16#B0#),
        Character'Val (16#20#),
        Character'Val (16#E2#),
        Character'Val (16#8A#),
        Character'Val (16#97#),
        Character'Val (16#E2#),
        Character'Val (16#88#),
        Character'Val (16#AC#),
        Character'Val (16#20#),
        Character'Val (16#F0#),
        Character'Val (16#9D#),
        Character'Val (16#9B#),
        Character'Val (16#BB#),
        Character'Val (16#F0#),
        Character'Val (16#9D#),
        Character'Val (16#9C#),
        Character'Val (16#95#),
        Character'Val (16#20#)));

   --  "ASCII Кириллица ⊗∬ 𝛻𝜕 "
   S2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String
       ((Character'Val (16#41#),
        Character'Val (16#53#),
        Character'Val (16#43#),
        Character'Val (16#49#),
        Character'Val (16#49#),
        Character'Val (16#20#),
        Character'Val (16#D0#),
        Character'Val (16#9A#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#80#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#86#),
        Character'Val (16#D0#),
        Character'Val (16#B0#),
        Character'Val (16#20#),
        Character'Val (16#E2#),
        Character'Val (16#8A#),
        Character'Val (16#97#),
        Character'Val (16#E2#),
        Character'Val (16#88#),
        Character'Val (16#AC#),
        Character'Val (16#20#),
        Character'Val (16#F0#),
        Character'Val (16#9D#),
        Character'Val (16#9B#),
        Character'Val (16#BB#),
        Character'Val (16#F0#),
        Character'Val (16#9D#),
        Character'Val (16#9C#),
        Character'Val (16#95#),
        Character'Val (16#20#)));

   --  "ASCII КириллицА ⊗∬ 𝛻𝜕 "
   SA : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String
       ((Character'Val (16#41#),
        Character'Val (16#53#),
        Character'Val (16#43#),
        Character'Val (16#49#),
        Character'Val (16#49#),
        Character'Val (16#20#),
        Character'Val (16#D0#),
        Character'Val (16#9A#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#80#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#86#),
        Character'Val (16#D0#),
        Character'Val (16#90#),
        Character'Val (16#20#),
        Character'Val (16#E2#),
        Character'Val (16#8A#),
        Character'Val (16#97#),
        Character'Val (16#E2#),
        Character'Val (16#88#),
        Character'Val (16#AC#),
        Character'Val (16#20#),
        Character'Val (16#F0#),
        Character'Val (16#9D#),
        Character'Val (16#9B#),
        Character'Val (16#BB#),
        Character'Val (16#F0#),
        Character'Val (16#9D#),
        Character'Val (16#9C#),
        Character'Val (16#95#),
        Character'Val (16#20#)));

   SE1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String ("");

   SE2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String ("");

   SD1 : VSS.Strings.Virtual_String;
   pragma Warnings (Off, SD1);
   SD2 : VSS.Strings.Virtual_String;
   pragma Warnings (Off, SD2);

   --  "ASCII"
   Prefix_1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String ("ASCII");
   --  "Кириллица"
   Prefix_2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String
       ((Character'Val (16#D0#),
        Character'Val (16#9A#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#80#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#BB#),
        Character'Val (16#D0#),
        Character'Val (16#B8#),
        Character'Val (16#D1#),
        Character'Val (16#86#),
        Character'Val (16#D0#),
        Character'Val (16#B0#)));

   --  "𝜕 "
   Suffix_1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Virtual_String
       ((Character'Val (16#F0#),
        Character'Val (16#9D#),
        Character'Val (16#9C#),
        Character'Val (16#95#),
        Character'Val (16#20#)));

begin
   ---------
   -- "=" --
   ---------

   if SD1 /= SD2 then
      raise Program_Error;
   end if;

   if SE1 /= SE2 then
      raise Program_Error;
   end if;

   if SE1 /= SD1 then
      raise Program_Error;
   end if;

   if S1 /= S2 then
      raise Program_Error;
   end if;

   if SD1 /= SD1 then
      raise Program_Error;
   end if;

   if SE1 /= SE1 then
      raise Program_Error;
   end if;

   if S1 /= S1 then
      raise Program_Error;
   end if;

   if S1 = SA then
      raise Program_Error;
   end if;

   ---------
   -- "<" --
   ---------

   if SD1 < SD2 then
      raise Program_Error;
   end if;

   if SE1 < SE2 then
      raise Program_Error;
   end if;

   if SD1 < SE1 then
      raise Program_Error;
   end if;

   if SE1 < SD1 then
      raise Program_Error;
   end if;

   if S1 < SD1 then
      raise Program_Error;
   end if;

   if not (SD1 < S1) then
      raise Program_Error;
   end if;

   if S1 < SE1 then
      raise Program_Error;
   end if;

   if not (SE1 < S1) then
      raise Program_Error;
   end if;

   if S1 < SA then
      raise Program_Error;
   end if;

   if not (SA < S1) then
      raise Program_Error;
   end if;

   if S1 < S2 then
      raise Program_Error;
   end if;

   ----------
   -- "<=" --
   ----------

   if not (SD1 <= SD2) then
      raise Program_Error;
   end if;

   if not (SE1 <= SE2) then
      raise Program_Error;
   end if;

   if not (SD1 <= SE1) then
      raise Program_Error;
   end if;

   if not (SE1 <= SD1) then
      raise Program_Error;
   end if;

   if S1 <= SD1 then
      raise Program_Error;
   end if;

   if not (SD1 <= S1) then
      raise Program_Error;
   end if;

   if S1 <= SE1 then
      raise Program_Error;
   end if;

   if not (SE1 <= S1) then
      raise Program_Error;
   end if;

   if S1 <= SA then
      raise Program_Error;
   end if;

   if not (SA <= S1) then
      raise Program_Error;
   end if;

   if not (S1 <= S2) then
      raise Program_Error;
   end if;

   ---------
   -- ">" --
   ---------

   if SD1 > SD2 then
      raise Program_Error;
   end if;

   if SE1 > SE2 then
      raise Program_Error;
   end if;

   if SD1 > SE1 then
      raise Program_Error;
   end if;

   if SE1 > SD1 then
      raise Program_Error;
   end if;

   if not (S1 > SD1) then
      raise Program_Error;
   end if;

   if SD1 > S1 then
      raise Program_Error;
   end if;

   if not (S1 > SE1) then
      raise Program_Error;
   end if;

   if SE1 > S1 then
      raise Program_Error;
   end if;

   if not (S1 > SA) then
      raise Program_Error;
   end if;

   if SA > S1 then
      raise Program_Error;
   end if;

   if S1 > S2 then
      raise Program_Error;
   end if;

   ----------
   -- ">=" --
   ----------

   if not (SD1 >= SD2) then
      raise Program_Error;
   end if;

   if not (SE1 >= SE2) then
      raise Program_Error;
   end if;

   if not (SD1 >= SE1) then
      raise Program_Error;
   end if;

   if not (SE1 >= SD1) then
      raise Program_Error;
   end if;

   if not (S1 >= SD1) then
      raise Program_Error;
   end if;

   if SD1 >= S1 then
      raise Program_Error;
   end if;

   if not (S1 >= SE1) then
      raise Program_Error;
   end if;

   if SE1 >= S1 then
      raise Program_Error;
   end if;

   if not (S1 >= SA) then
      raise Program_Error;
   end if;

   if SA >= S1 then
      raise Program_Error;
   end if;

   if not (S1 >= S2) then
      raise Program_Error;
   end if;

   ------------
   -- Starts --
   ------------

   if not SD1.Starts_With (SD2) then
      raise Program_Error;
   end if;

   if not SD1.Starts_With (SE1) then
      raise Program_Error;
   end if;

   if SD1.Starts_With (Prefix_1) then
      raise Program_Error;
   end if;

   if SD1.Starts_With (Prefix_2) then
      raise Program_Error;
   end if;

   if not SE1.Starts_With (SD1) then
      raise Program_Error;
   end if;

   if not SE1.Starts_With (SE1) then
      raise Program_Error;
   end if;

   if not SE1.Starts_With (SE2) then
      raise Program_Error;
   end if;

   if not S1.Starts_With (SD1) then
      raise Program_Error;
   end if;

   if not S1.Starts_With (SE1) then
      raise Program_Error;
   end if;

   if not S1.Starts_With (Prefix_1) then
      raise Program_Error;
   end if;

   if S1.Starts_With (Prefix_2) then
      raise Program_Error;
   end if;

   if Prefix_1.Starts_With (S1) then
      raise Program_Error;
   end if;

   if Prefix_2.Starts_With (S1) then
      raise Program_Error;
   end if;

   ----------
   -- Ends --
   ----------

   if not SD1.Ends_With (SD2) then
      raise Program_Error;
   end if;

   if not SD1.Ends_With (SE1) then
      raise Program_Error;
   end if;

   if SD1.Ends_With (Prefix_1) then
      raise Program_Error;
   end if;

   if SD1.Ends_With (Prefix_2) then
      raise Program_Error;
   end if;

   if not SE1.Ends_With (SD1) then
      raise Program_Error;
   end if;

   if not SE1.Ends_With (SE1) then
      raise Program_Error;
   end if;

   if not SE1.Ends_With (SE2) then
      raise Program_Error;
   end if;

   if not S1.Ends_With (SD1) then
      raise Program_Error;
   end if;

   if not S1.Ends_With (SE1) then
      raise Program_Error;
   end if;

   if not S1.Ends_With (Suffix_1) then
      raise Program_Error;
   end if;

   if S1.Ends_With (Prefix_2) then
      raise Program_Error;
   end if;

   if Prefix_1.Ends_With (S1) then
      raise Program_Error;
   end if;

   if Prefix_2.Ends_With (S1) then
      raise Program_Error;
   end if;

end Test_String_Compare;
