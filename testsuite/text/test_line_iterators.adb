------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with VSS.Strings.Line_Iterators;

procedure Test_Line_Iterators is

   LF  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000A#);
   VT  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000B#);
   FF  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000C#);
   CR  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_000D#);
   NEL : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_0085#);
   LS  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_2028#);
   PS  : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#00_2029#);

   type Expected_Record is record
      Line_First_Character       : VSS.Strings.Character_Count;
      Line_Last_Character        : VSS.Strings.Character_Count;
      Terminator_First_Character : VSS.Strings.Character_Count;
      Terminator_Last_Character  : VSS.Strings.Character_Count;
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
     (1 => (1, 1, 2, 2),
      2 => (3, 2, 3, 3),
      3 => (4, 10, 11, 11),
      4 => (12, 11, 12, 12),
      5 => (13, 42, 43, 44),
      6 => (45, 44, 45, 46),
      7 => (47, 47, 48, 48),
      8 => (49, 48, 49, 49),
      9 => (50, 56, 57, 56));

   Expected_1_2 : constant Expected_Array :=
     (1 => (1, 2, 2, 2),
      2 => (3, 3, 3, 3),
      3 => (4, 11, 11, 11),
      4 => (12, 12, 12, 12),
      5 => (13, 44, 43, 44),
      6 => (45, 46, 45, 46),
      7 => (47, 48, 48, 48),
      8 => (49, 49, 49, 49),
      9 => (50, 56, 57, 56));

   Expected_1_3 : constant Expected_Array :=
     (1 => (1, 1, 2, 2),
      2 => (3, 2, 3, 3),
      3 => (4, 4, 5, 5),
      4 => (6, 5, 6, 6),
      5 => (7, 7, 8, 8),
      6 => (9, 8, 9, 9),
      7 => (10, 10, 11, 11),
      8 => (12, 11, 12, 12),
      9 => (13, 42, 43, 44),
      10 => (45, 44, 45, 46),
      11 => (47, 47, 48, 48),
      12 => (49, 48, 49, 49),
      13 => (50, 50, 51, 51),
      14 => (52, 51, 52, 52),
      15 => (53, 53, 54, 54),
      16 => (55, 54, 55, 55),
      17 => (56, 56, 57, 56));

   Expected_1_4 : constant Expected_Array :=
     (1 => (1, 2, 2, 2),
      2 => (3, 3, 3, 3),
      3 => (4, 5, 5, 5),
      4 => (6, 6, 6, 6),
      5 => (7, 8, 8, 8),
      6 => (9, 9, 9, 9),
      7 => (10, 11, 11, 11),
      8 => (12, 12, 12, 12),
      9 => (13, 44, 43, 44),
      10 => (45, 46, 45, 46),
      11 => (47, 48, 48, 48),
      12 => (49, 49, 49, 49),
      13 => (50, 51, 51, 51),
      14 => (52, 52, 52, 52),
      15 => (53, 54, 54, 54),
      16 => (55, 55, 55, 55),
      17 => (56, 56, 57, 56));

   CRLFCR : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("a" & CR & LF & CR
        & "b" & CR & LF & CR);

   Expected_2_1 : constant Expected_Array :=
     (1 => (1, 1, 2, 3),
      2 => (4, 3, 4, 4),
      3 => (5, 5, 6, 7),
      4 => (8, 7, 8, 8));

   Expected_2_2 : constant Expected_Array :=
     (1 => (1, 1, 2, 2),
      2 => (3, 2, 3, 3),
      3 => (4, 3, 4, 4),
      4 => (5, 5, 6, 6),
      5 => (7, 6, 7, 7),
      6 => (8, 7, 8, 8));

   Expected_2_3 : constant Expected_Array :=
     (1 => (1, 1, 2, 3),
      2 => (4, 5, 6, 7),
      3 => (8, 8, 9, 8));

   procedure Test_Forward
     (Source_String   : VSS.Strings.Virtual_String;
      Expected_Result : Expected_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean);

   ------------------
   -- Test_Forward --
   ------------------

   procedure Test_Forward
     (Source_String   : VSS.Strings.Virtual_String;
      Expected_Result : Expected_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean)
   is
      use type VSS.Strings.Character_Count;

      J : VSS.Strings.Line_Iterators.Line_Iterator :=
        Source_String.First_Line (Terminators, Keep_Terminator);
      C : Natural := 1;

   begin
      loop
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

         exit when not J.Forward;

         C := C + 1;
      end loop;

      if C /= Expected_Result'Length then
         raise Program_Error;
      end if;

      if J.Forward then
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
   end Test_Forward;

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
end Test_Line_Iterators;
