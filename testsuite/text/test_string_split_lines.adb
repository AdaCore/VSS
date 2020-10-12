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

with VSS.String_Vectors;
with VSS.Strings;

procedure Test_String_Split_Lines is

   LF  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000A#);
   VT  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000B#);
   FF  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000C#);
   CR  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000D#);
   NEL : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_0085#);
   LS  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_2028#);
   PS  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_2029#);

   --  XXX add check for null string

   Source : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("a" & LF & LF
        & "b" & VT & VT
        & "c" & FF & FF
        & "d" & CR & CR
        & "e it is intentionaly long line" & CR & LF & CR & LF
        & "f" & NEL & NEL
        & "g" & LS & LS
        & "h" & PS & PS
        & "z");

   Small_Source : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("a" & LF
        & "d" & CR
        & "e" & CR & LF
        & "f" & NEL);

begin
   --  Check split lines with default set of line terminators and with strip
   --  out of line terminator sequences.

   declare
      use type VSS.Strings.Virtual_String;

      Result : constant VSS.String_Vectors.Virtual_String_Vector :=
        Source.Split_Lines;

   begin
      if Result.Length /= 9 then
         raise Program_Error;
      end if;

      if Result (1) /= VSS.Strings.To_Virtual_String ("a") then
         raise Program_Error;
      end if;

      if Result (2) /= VSS.Strings.Empty_Virtual_String then
         raise Program_Error;
      end if;

      if Result (3)
        /= VSS.Strings.To_Virtual_String ("b" & VT & VT & "c" & FF & FF & "d")
      then
         raise Program_Error;
      end if;

      if Result (4) /= VSS.Strings.Empty_Virtual_String then
         raise Program_Error;
      end if;

      if Result (5)
        /= VSS.Strings.To_Virtual_String ("e it is intentionaly long line")
      then
         raise Program_Error;
      end if;

      if Result (6) /= VSS.Strings.Empty_Virtual_String then
         raise Program_Error;
      end if;

      if Result (7) /= VSS.Strings.To_Virtual_String ("f") then
         raise Program_Error;
      end if;

      if Result (8) /= VSS.Strings.Empty_Virtual_String then
         raise Program_Error;
      end if;

      if Result (9)
        /= VSS.Strings.To_Virtual_String ("g" & LS & LS & "h" & PS & PS & "z")
      then
         raise Program_Error;
      end if;
   end;

   --  Check split lines with default set of line terminators and with
   --  preserve of line terminator sequences.

   declare
      use type VSS.Strings.Virtual_String;

      Result : constant VSS.String_Vectors.Virtual_String_Vector :=
        Source.Split_Lines (Keep_Terminator => True);

   begin
      if Result.Length /= 9 then
         raise Program_Error;
      end if;

      if Result (1) /= VSS.Strings.To_Virtual_String ("a" & LF) then
         raise Program_Error;
      end if;

      if Result (2) /= VSS.Strings.To_Virtual_String ((1 => LF)) then
         raise Program_Error;
      end if;

      if Result (3)
        /= VSS.Strings.To_Virtual_String
             ("b" & VT & VT & "c" & FF & FF & "d" & CR)
      then
         raise Program_Error;
      end if;

      if Result (4) /= VSS.Strings.To_Virtual_String ((1 => CR)) then
         raise Program_Error;
      end if;

      if Result (5) /=
        VSS.Strings.To_Virtual_String
          ("e it is intentionaly long line" & CR & LF)
      then
         raise Program_Error;
      end if;

      if Result (6) /= VSS.Strings.To_Virtual_String (CR & LF) then
         raise Program_Error;
      end if;

      if Result (7)
        /= VSS.Strings.To_Virtual_String ("f" & NEL)
      then
         raise Program_Error;
      end if;

      if Result (8) /= VSS.Strings.To_Virtual_String ((1 => NEL)) then
         raise Program_Error;
      end if;

      if Result (9)
        /= VSS.Strings.To_Virtual_String ("g" & LS & LS & "h" & PS & PS & "z")
      then
         raise Program_Error;
      end if;
   end;

   --  Check split lines with full set of line terminators and with strip out
   --  of line terminator sequences.

   declare
      use type VSS.Strings.Virtual_String;

      Result : constant VSS.String_Vectors.Virtual_String_Vector :=
        Source.Split_Lines ((others => True));

   begin
      if Result.Length /= 17 then
         raise Program_Error;
      end if;

      if Result (1) /= VSS.Strings.To_Virtual_String ("a") then
         raise Program_Error;
      end if;

      if not Result (2).Is_Empty then
         raise Program_Error;
      end if;

      if Result (3) /= VSS.Strings.To_Virtual_String ("b") then
         raise Program_Error;
      end if;

      if not Result (4).Is_Empty then
         raise Program_Error;
      end if;

      if Result (5) /= VSS.Strings.To_Virtual_String ("c") then
         raise Program_Error;
      end if;

      if not Result (6).Is_Empty then
         raise Program_Error;
      end if;

      if Result (7) /= VSS.Strings.To_Virtual_String ("d") then
         raise Program_Error;
      end if;

      if not Result (8).Is_Empty then
         raise Program_Error;
      end if;

      if Result (9) /=
        VSS.Strings.To_Virtual_String ("e it is intentionaly long line")
      then
         raise Program_Error;
      end if;

      if not Result (10).Is_Empty then
         raise Program_Error;
      end if;

      if Result (11) /= VSS.Strings.To_Virtual_String ("f") then
         raise Program_Error;
      end if;

      if not Result (12).Is_Empty then
         raise Program_Error;
      end if;

      if Result (13) /= VSS.Strings.To_Virtual_String ("g") then
         raise Program_Error;
      end if;

      if not Result (14).Is_Empty then
         raise Program_Error;
      end if;

      if Result (15) /= VSS.Strings.To_Virtual_String ("h") then
         raise Program_Error;
      end if;

      if not Result (16).Is_Empty then
         raise Program_Error;
      end if;

      if Result (17) /= VSS.Strings.To_Virtual_String ("z") then
         raise Program_Error;
      end if;
   end;

   --  Check split lines with default set of line terminators and with strip
   --  out of line terminator sequences.

   declare
      use type VSS.Strings.Virtual_String;

      Result : constant VSS.String_Vectors.Virtual_String_Vector :=
        Small_Source.Split_Lines;

   begin
      if Result.Length /= 4 then
         raise Program_Error;
      end if;

      if Result (1) /= VSS.Strings.To_Virtual_String ("a") then
         raise Program_Error;
      end if;

      if Result (2) /= VSS.Strings.To_Virtual_String ("d") then
         raise Program_Error;
      end if;

      if Result (3) /= VSS.Strings.To_Virtual_String ("e") then
         raise Program_Error;
      end if;

      if Result (4) /= VSS.Strings.To_Virtual_String ("f") then
         raise Program_Error;
      end if;
   end;
end Test_String_Split_Lines;
