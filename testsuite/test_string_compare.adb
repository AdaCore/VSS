------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with VSS.Strings.Conversions;

procedure Test_String_Compare is

   use type VSS.Strings.Virtual_String;

   --  "ASCII Кириллица ⊗∬ 𝛻𝜕 "
   S1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String
       ((Character'Val(16#41#),
        Character'Val(16#53#),
        Character'Val(16#43#),
        Character'Val(16#49#),
        Character'Val(16#49#),
        Character'Val(16#20#),
        Character'Val(16#D0#),
        Character'Val(16#9A#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#80#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#86#),
        Character'Val(16#D0#),
        Character'Val(16#B0#),
        Character'Val(16#20#),
        Character'Val(16#E2#),
        Character'Val(16#8A#),
        Character'Val(16#97#),
        Character'Val(16#E2#),
        Character'Val(16#88#),
        Character'Val(16#AC#),
        Character'Val(16#20#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9B#),
        Character'Val(16#BB#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9C#),
        Character'Val(16#95#),
        Character'Val(16#20#)));

   --  "ASCII Кириллица ⊗∬ 𝛻𝜕 "
   S2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String
       ((Character'Val(16#41#),
        Character'Val(16#53#),
        Character'Val(16#43#),
        Character'Val(16#49#),
        Character'Val(16#49#),
        Character'Val(16#20#),
        Character'Val(16#D0#),
        Character'Val(16#9A#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#80#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#86#),
        Character'Val(16#D0#),
        Character'Val(16#B0#),
        Character'Val(16#20#),
        Character'Val(16#E2#),
        Character'Val(16#8A#),
        Character'Val(16#97#),
        Character'Val(16#E2#),
        Character'Val(16#88#),
        Character'Val(16#AC#),
        Character'Val(16#20#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9B#),
        Character'Val(16#BB#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9C#),
        Character'Val(16#95#),
        Character'Val(16#20#)));

   --  "ASCII КириллицА ⊗∬ 𝛻𝜕 "
   SA : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String
       ((Character'Val(16#41#),
        Character'Val(16#53#),
        Character'Val(16#43#),
        Character'Val(16#49#),
        Character'Val(16#49#),
        Character'Val(16#20#),
        Character'Val(16#D0#),
        Character'Val(16#9A#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#80#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#86#),
        Character'Val(16#D0#),
        Character'Val(16#90#),
        Character'Val(16#20#),
        Character'Val(16#E2#),
        Character'Val(16#8A#),
        Character'Val(16#97#),
        Character'Val(16#E2#),
        Character'Val(16#88#),
        Character'Val(16#AC#),
        Character'Val(16#20#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9B#),
        Character'Val(16#BB#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9C#),
        Character'Val(16#95#),
        Character'Val(16#20#)));

   SE1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String ("");

   SE2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String ("");

   SD1 : VSS.Strings.Virtual_String;
   pragma Warnings (Off, SD1);
   SD2 : VSS.Strings.Virtual_String;
   pragma Warnings (Off, SD2);

   --  "ASCII"
   Prefix_1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String ("ASCII");
   --  "Кириллица"
   Prefix_2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String
       ((Character'Val(16#D0#),
        Character'Val(16#9A#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#80#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#86#),
        Character'Val(16#D0#),
        Character'Val(16#B0#)));

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

   if not SD1.Starts (SD2) then
      raise Program_Error;
   end if;

   if not SD1.Starts (SE1) then
      raise Program_Error;
   end if;

   if SD1.Starts (Prefix_1) then
      raise Program_Error;
   end if;

   if SD1.Starts (Prefix_2) then
      raise Program_Error;
   end if;

   if not SE1.Starts (SD1) then
      raise Program_Error;
   end if;

   if not SE1.Starts (SE1) then
      raise Program_Error;
   end if;

   if not SE1.Starts (SE2) then
      raise Program_Error;
   end if;

   if not S1.Starts (SD1) then
      raise Program_Error;
   end if;

   if not S1.Starts (SE1) then
      raise Program_Error;
   end if;

   if not S1.Starts (Prefix_1) then
      raise Program_Error;
   end if;

   if S1.Starts (Prefix_2) then
      raise Program_Error;
   end if;

   if Prefix_1.Starts (S1) then
      raise Program_Error;
   end if;

   if Prefix_2.Starts (S1) then
      raise Program_Error;
   end if;

end Test_String_Compare;
