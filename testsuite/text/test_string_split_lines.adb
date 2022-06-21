--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   CRLFCR : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("a" & CR & LF & CR
        & "b" & CR & LF & CR);

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

   --  Check CRLF + CR when both line terminators are enabled

   declare
      use type VSS.Strings.Virtual_String;

      Result : constant VSS.String_Vectors.Virtual_String_Vector :=
        CRLFCR.Split_Lines;

   begin
      if Result.Length /= 4 then
         raise Program_Error;
      end if;

      if Result.Element (1) /= VSS.Strings.To_Virtual_String ("a") then
         raise Program_Error;
      end if;

      if not Result.Element (2).Is_Empty then
         raise Program_Error;
      end if;

      if Result.Element (3) /= VSS.Strings.To_Virtual_String ("b") then
         raise Program_Error;
      end if;

      if not Result.Element (4).Is_Empty then
         raise Program_Error;
      end if;
   end;

   --  Check CR + LF when CRLF is disabled

   declare
      use type VSS.Strings.Virtual_String;

      Result : constant VSS.String_Vectors.Virtual_String_Vector :=
        CRLFCR.Split_Lines
          ((VSS.Strings.CR | VSS.Strings.LF => True, others => False));

   begin
      if Result.Length /= 6 then
         raise Program_Error;
      end if;

      if Result.Element (1) /= VSS.Strings.To_Virtual_String ("a") then
         raise Program_Error;
      end if;

      if not Result.Element (2).Is_Empty then
         raise Program_Error;
      end if;

      if not Result.Element (3).Is_Empty then
         raise Program_Error;
      end if;

      if Result.Element (4) /= VSS.Strings.To_Virtual_String ("b") then
         raise Program_Error;
      end if;

      if not Result.Element (5).Is_Empty then
         raise Program_Error;
      end if;

      if not Result.Element (6).Is_Empty then
         raise Program_Error;
      end if;
   end;
end Test_String_Split_Lines;
