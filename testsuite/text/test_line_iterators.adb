--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Character_Iterators;
with VSS.Strings.Line_Iterators;
with VSS.Strings.Markers;

with Test_Support;

procedure Test_Line_Iterators is

   use type VSS.Strings.Character_Count;

   LF  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000A#);
   VT  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000B#);
   FF  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000C#);
   CR  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000D#);
   NEL : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_0085#);
   LS  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_2028#);
   PS  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_2029#);

   LF_String   : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => LF));
   VT_String   : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => VT));
   FF_String   : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => FF));
   CR_String   : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => CR));
   CRLF_String : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => CR, 2 => LF));
   NEL_String  : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => NEL));
   LS_String   : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => LS));
   PS_String   : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ((1 => PS));

   type Expected_Record is record
      Line_First_Character       : VSS.Strings.Character_Count;
      Line_Last_Character        : VSS.Strings.Character_Count;
      Terminator_First_Character : VSS.Strings.Character_Count;
      Terminator_Last_Character  : VSS.Strings.Character_Count;
      Has_Line_Terminator        : Boolean;
      Terminator_String          : VSS.Strings.Virtual_String;
   end record;

   type Expected_Array is array (Positive range <>) of Expected_Record;

   Source_1 : constant VSS.Strings.Virtual_String :=
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

   Expected_1_1 : constant Expected_Array :=
     (1 => (1, 1, 2, 2, True, LF_String),
      2 => (3, 2, 3, 3, True, LF_String),
      3 => (4, 10, 11, 11, True, CR_String),
      4 => (12, 11, 12, 12, True, CR_String),
      5 => (13, 42, 43, 44, True, CRLF_String),
      6 => (45, 44, 45, 46, True, CRLF_String),
      7 => (47, 47, 48, 48, True, NEL_String),
      8 => (49, 48, 49, 49, True, NEL_String),
      9 => (50, 56, 57, 56, False, VSS.Strings.Empty_Virtual_String));

   Expected_1_2 : constant Expected_Array :=
     (1 => (1, 2, 2, 2, True, LF_String),
      2 => (3, 3, 3, 3, True, LF_String),
      3 => (4, 11, 11, 11, True, CR_String),
      4 => (12, 12, 12, 12, True, CR_String),
      5 => (13, 44, 43, 44, True, CRLF_String),
      6 => (45, 46, 45, 46, True, CRLF_String),
      7 => (47, 48, 48, 48, True, NEL_String),
      8 => (49, 49, 49, 49, True, NEL_String),
      9 => (50, 56, 57, 56, False, VSS.Strings.Empty_Virtual_String));

   Expected_1_3 : constant Expected_Array :=
     (1 => (1, 1, 2, 2, True, LF_String),
      2 => (3, 2, 3, 3, True, LF_String),
      3 => (4, 4, 5, 5, True, VT_String),
      4 => (6, 5, 6, 6, True, VT_String),
      5 => (7, 7, 8, 8, True, FF_String),
      6 => (9, 8, 9, 9, True, FF_String),
      7 => (10, 10, 11, 11, True, CR_String),
      8 => (12, 11, 12, 12, True, CR_String),
      9 => (13, 42, 43, 44, True, CRLF_String),
      10 => (45, 44, 45, 46, True, CRLF_String),
      11 => (47, 47, 48, 48, True, NEL_String),
      12 => (49, 48, 49, 49, True, NEL_String),
      13 => (50, 50, 51, 51, True, LS_String),
      14 => (52, 51, 52, 52, True, LS_String),
      15 => (53, 53, 54, 54, True, PS_String),
      16 => (55, 54, 55, 55, True, PS_String),
      17 => (56, 56, 57, 56, False, VSS.Strings.Empty_Virtual_String));

   Expected_1_4 : constant Expected_Array :=
     (1 => (1, 2, 2, 2, True, LF_String),
      2 => (3, 3, 3, 3, True, LF_String),
      3 => (4, 5, 5, 5, True, VT_String),
      4 => (6, 6, 6, 6, True, VT_String),
      5 => (7, 8, 8, 8, True, FF_String),
      6 => (9, 9, 9, 9, True, FF_String),
      7 => (10, 11, 11, 11, True, CR_String),
      8 => (12, 12, 12, 12, True, CR_String),
      9 => (13, 44, 43, 44, True, CRLF_String),
      10 => (45, 46, 45, 46, True, CRLF_String),
      11 => (47, 48, 48, 48, True, NEL_String),
      12 => (49, 49, 49, 49, True, NEL_String),
      13 => (50, 51, 51, 51, True, LS_String),
      14 => (52, 52, 52, 52, True, LS_String),
      15 => (53, 54, 54, 54, True, PS_String),
      16 => (55, 55, 55, 55, True, PS_String),
      17 => (56, 56, 57, 56, False, VSS.Strings.Empty_Virtual_String));

   CRLFCR : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("a" & CR & LF & CR
        & "b" & CR & LF & CR);

   Expected_2_1 : constant Expected_Array :=
     (1 => (1, 1, 2, 3, True, CRLF_String),
      2 => (4, 3, 4, 4, True, CR_String),
      3 => (5, 5, 6, 7, True, CRLF_String),
      4 => (8, 7, 8, 8, True, CR_String));

   Expected_2_2 : constant Expected_Array :=
     (1 => (1, 1, 2, 2, True, CR_String),
      2 => (3, 2, 3, 3, True, LF_String),
      3 => (4, 3, 4, 4, True, CR_String),
      4 => (5, 5, 6, 6, True, CR_String),
      5 => (7, 6, 7, 7, True, LF_String),
      6 => (8, 7, 8, 8, True, CR_String));

   Expected_2_3 : constant Expected_Array :=
     (1 => (1, 1, 2, 3, True, CRLF_String),
      2 => (4, 5, 6, 7, True, CRLF_String),
      3 => (8, 8, 9, 8, False, VSS.Strings.Empty_Virtual_String));

   Pack : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("package Pack is" & LF);
   --  Text of single line with line terminator.

   Expected_3 : constant Expected_Array :=
     (1 => (1, 16, 16, 16, True, LF_String));

   procedure Test_Forward
     (Source_String   : VSS.Strings.Virtual_String;
      Expected_Result : Expected_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean);

   procedure Test_Forward_Restart
     (Source_String   : VSS.Strings.Virtual_String;
      Expected_Result : Expected_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Restart_Line    : Positive);

   procedure Test_U902_007;
   --  Run test of line terminator sequence for single line without line
   --  terminator.

   ------------------
   -- Test_Forward --
   ------------------

   procedure Test_Forward
     (Source_String   : VSS.Strings.Virtual_String;
      Expected_Result : Expected_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean)
   is
      use type VSS.Strings.Virtual_String;

      J : VSS.Strings.Line_Iterators.Line_Iterator :=
        Source_String.At_First_Line (Terminators, Keep_Terminator);
      C : Natural := 1;

   begin
      loop
         if not J.Has_Element then
            raise Program_Error;
         end if;

         if J.First_Character_Index
           /= Expected_Result (C).Line_First_Character
         then
            raise Program_Error;
         end if;

         if J.Last_Character_Index
           /= Expected_Result (C).Line_Last_Character
         then
            raise Program_Error;
         end if;

         if J.Terminator_First_Character_Index
           /= Expected_Result (C).Terminator_First_Character
         then
            raise Program_Error;
         end if;

         if J.Terminator_Last_Character_Index
           /= Expected_Result (C).Terminator_Last_Character
         then
            raise Program_Error;
         end if;

         if J.Has_Line_Terminator
           /= Expected_Result (C).Has_Line_Terminator
         then
            raise Program_Error;
         end if;

         Test_Support.Assert
           (J.Element_Terminator = Expected_Result (C).Terminator_String);

         exit when not J.Forward;

         C := C + 1;
      end loop;

      if J.Has_Element then
         raise Program_Error;
      end if;

      if C /= Expected_Result'Length then
         raise Program_Error;
      end if;

      if J.Forward then
         raise Program_Error;
      end if;

      if J.Has_Element then
         raise Program_Error;
      end if;

      if J.First_Character_Index /= Source_String.Character_Length + 1 then
         raise Program_Error;
      end if;

      if J.Last_Character_Index /= Source_String.Character_Length then
         raise Program_Error;
      end if;

      if J.Terminator_First_Character_Index
        /= Source_String.Character_Length + 1
      then
         raise Program_Error;
      end if;

      if J.Terminator_Last_Character_Index
        /= Source_String.Character_Length
      then
         raise Program_Error;
      end if;

      if J.Has_Line_Terminator /= False then
         raise Program_Error;
      end if;
   end Test_Forward;

   --------------------------
   -- Test_Forward_Restart --
   --------------------------

   procedure Test_Forward_Restart
     (Source_String   : VSS.Strings.Virtual_String;
      Expected_Result : Expected_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Restart_Line    : Positive)
   is
      --  J : VSS.Strings.Line_Iterators.Line_Iterator :=
      --    Source_String.First_Line (Terminators, Keep_Terminator);

      M : VSS.Strings.Markers.Character_Marker;

   begin
      declare
         JL : VSS.Strings.Line_Iterators.Line_Iterator :=
           Source_String.At_First_Line (Terminators, Keep_Terminator);

      begin
         for K in 2 .. Restart_Line loop
            Test_Support.Assert (JL.Forward);
         end loop;

         declare
            JC : VSS.Strings.Character_Iterators.Character_Iterator :=
              Source_String.At_Character (JL.First_Marker);

         begin
            for K in 1 .. JL.Character_Length / 2 loop
               Test_Support.Assert (JC.Forward);
            end loop;

            M := JC.Marker;
         end;
      end;

      declare
         J : VSS.Strings.Line_Iterators.Line_Iterator :=
           Source_String.At_Line (M, Terminators, Keep_Terminator);
         C : Positive := Restart_Line;

      begin
         loop
            Test_Support.Assert (J.Has_Element);
            Test_Support.Assert
              (J.First_Character_Index
                 = Expected_Result (C).Line_First_Character);
            Test_Support.Assert
              (J.Last_Character_Index
                 = Expected_Result (C).Line_Last_Character);
            Test_Support.Assert
              (J.Terminator_First_Character_Index
                 = Expected_Result (C).Terminator_First_Character);
            Test_Support.Assert
              (J.Terminator_Last_Character_Index
                 = Expected_Result (C).Terminator_Last_Character);
            Test_Support.Assert
              (J.Has_Line_Terminator
                 = Expected_Result (C).Has_Line_Terminator);

            exit when not J.Forward;

            C := C + 1;
         end loop;

         Test_Support.Assert (not J.Has_Element);
         Test_Support.Assert (C = Expected_Result'Length);

         Test_Support.Assert (not J.Forward);
         Test_Support.Assert (not J.Has_Element);

         Test_Support.Assert
           (J.First_Character_Index = Source_String.Character_Length + 1);

         Test_Support.Assert
           (J.Last_Character_Index = Source_String.Character_Length);

         Test_Support.Assert
           (J.Terminator_First_Character_Index
              = Source_String.Character_Length + 1);
         Test_Support.Assert
           (J.Terminator_Last_Character_Index
              = Source_String.Character_Length);
         Test_Support.Assert (not J.Has_Line_Terminator);
      end;
   end Test_Forward_Restart;

   -------------------
   -- Test_U902_007 --
   -------------------

   procedure Test_U902_007 is
      Text : constant VSS.Strings.Virtual_String := "f";
      J    : constant VSS.Strings.Line_Iterators.Line_Iterator :=
        Text.At_First_Line (Keep_Terminator => True);
      LT   : VSS.Strings.Virtual_String;

   begin
      Test_Support.Assert (J.Has_Element);

      LT := Text.Slice (J.Terminator_First_Marker, J.Terminator_Last_Marker);
      Test_Support.Assert (LT.Is_Empty);

      LT := J.Element_Terminator;
      Test_Support.Assert (LT.Is_Empty);
   end Test_U902_007;

begin
   Test_Forward (Source_1, Expected_1_1, VSS.Strings.New_Line_Function, False);
   Test_Forward (Source_1, Expected_1_2, VSS.Strings.New_Line_Function, True);
   Test_Forward (Source_1, Expected_1_3, (others => True), False);
   Test_Forward (Source_1, Expected_1_4, (others => True), True);

   Test_Forward (CRLFCR, Expected_2_1, VSS.Strings.New_Line_Function, False);
   Test_Forward
     (CRLFCR,
      Expected_2_2,
      (VSS.Strings.CR | VSS.Strings.LF => True, others => False),
      False);
   Test_Forward
     (CRLFCR,
      Expected_2_3,
      (VSS.Strings.CRLF => True, others => False),
      False);

   Test_Forward
     (Pack,
      Expected_3,
      (VSS.Strings.CR | VSS.Strings.LF | VSS.Strings.CRLF => True,
       others => False),
      True);

   for J in 2 .. Expected_1_2'Last loop
      Test_Forward_Restart
        (Source_1, Expected_1_2, VSS.Strings.New_Line_Function, True, J);
   end loop;

   for J in 2 .. Expected_1_4'Last loop
      Test_Forward_Restart
        (Source_1, Expected_1_4, (others => True), True, J);
   end loop;

   Test_U902_007;
end Test_Line_Iterators;
