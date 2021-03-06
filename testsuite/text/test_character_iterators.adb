------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Unicode;

with VSS.Strings.Cursors.Markers;

procedure Test_Character_Iterators is

   use type VSS.Characters.Virtual_Character;
   use type VSS.Strings.Character_Count;
   use type VSS.Unicode.UTF8_Code_Unit_Count;
   use type VSS.Unicode.UTF16_Code_Unit_Count;

   type Position_Data is record
      Character          : VSS.Characters.Virtual_Character;
      First_UTF8_Offset  : VSS.Unicode.UTF8_Code_Unit_Index;
      Last_UTF8_Offset   : VSS.Unicode.UTF8_Code_Unit_Index;
      First_UTF16_Offset : VSS.Unicode.UTF16_Code_Unit_Index;
      Last_UTF16_Offset  : VSS.Unicode.UTF16_Code_Unit_Index;
   end record;

   --  "ASCII Кириллица ⊗∬ 𝛻𝜕 "
   S : constant VSS.Strings.Virtual_String :=
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

   D : constant array (VSS.Strings.Character_Index range <>) of Position_Data
     :=
    (('A', 0, 0, 0, 0),       --  'A' 1
     ('S', 1, 1, 1, 1),       --  'S' 2
     ('C', 2, 2, 2, 2),       --  'C' 3
     ('I', 3, 3, 3, 3),       --  'I' 4
     ('I', 4, 4, 4, 4),       --  'I' 5
     (' ', 5, 5, 5, 5),       --  ' ' 6
     ('К', 6, 7, 6, 6),       --  'К' 7
     ('и', 8, 9, 7, 7),       --  'и' 8
     ('р', 10, 11, 8, 8),     --  'р' 9
     ('и', 12, 13, 9, 9),     --  'и' 10
     ('л', 14, 15, 10, 10),   --  'л' 11
     ('л', 16, 17, 11, 11),   --  'л' 12
     ('и', 18, 19, 12, 12),   --  'и' 13
     ('ц', 20, 21, 13, 13),   --  'ц' 14
     ('а', 22, 23, 14, 14),   --  'а' 15
     (' ', 24, 24, 15, 15),   --  ' ' 16
     ('⊗', 25, 27, 16, 16),   --  '⊗' 17
     ('∬', 28, 30, 17, 17),   --  '∬' 18
     (' ', 31, 31, 18, 18),   --  ' ' 19
     ('𝛻', 32, 35, 19, 20),   --  '𝛻' 17
     ('𝜕', 36, 39, 21, 22),   --  '𝜕' 18
     (' ', 40, 40, 23, 23));  --  ' ' 19

   procedure Test_Forward;
   procedure Test_Backward;

   -------------------
   -- Test_Backward --
   -------------------

   procedure Test_Backward is
      J : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.Last_Character;
      C : VSS.Strings.Character_Count := D'Last;
      M : VSS.Strings.Cursors.Markers.Character_Marker;

   begin
      loop
         --  Check position of the cursor

         if not J.Has_Element then
            raise Program_Error;
         end if;

         if C /= J.Character_Index then
            raise Program_Error;
         end if;

         if J.Character_Index not in D'Range then
            raise Program_Error;
         end if;

         if J.Element /= D (C).Character then
            raise Program_Error;
         end if;

         if J.First_UTF8_Offset /= D (C).First_UTF8_Offset then
            raise Program_Error;
         end if;

         if J.Last_UTF8_Offset /= D (C).Last_UTF8_Offset then
            raise Program_Error;
         end if;

         if J.First_UTF16_Offset /= D (C).First_UTF16_Offset then
            raise Program_Error;
         end if;

         if J.Last_UTF16_Offset /= D (C).Last_UTF16_Offset then
            raise Program_Error;
         end if;

         --  Create mark and check its position

         M := J.Marker;

         if M.Character_Index /= C then
            raise Program_Error;
         end if;

         if M.Character_Index not in D'Range then
            raise Program_Error;
         end if;

         --  if M.Element /= D (C).Character then
         --     raise Program_Error;
         --  end if;

         --  GNAT 20210228: subprograms of Abstract_Cursor interface is not
         --  visible, thus implicit conversion is used.

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).First_UTF8_Offset
           /= D (C).First_UTF8_Offset
         then
            raise Program_Error;
         end if;

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).Last_UTF8_Offset
           /= D (C).Last_UTF8_Offset
         then
            raise Program_Error;
         end if;

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).First_UTF16_Offset
           /= D (C).First_UTF16_Offset
         then
            raise Program_Error;
         end if;

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).Last_UTF16_Offset
           /= D (C).Last_UTF16_Offset
         then
            raise Program_Error;
         end if;

         --  Create iterators from the iterator and from the mark and check
         --  their position

         declare
            J1 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
              S.Character (J);
            J2 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
              S.Character (M);

         begin
            if not J1.Has_Element then
               raise Program_Error;
            end if;

            if J1.Character_Index /= C then
               raise Program_Error;
            end if;

            if J1.Character_Index not in D'Range then
               raise Program_Error;
            end if;

            if J1.Element /= D (C).Character then
               raise Program_Error;
            end if;

            if J1.First_UTF8_Offset /= D (C).First_UTF8_Offset then
               raise Program_Error;
            end if;

            if J1.Last_UTF8_Offset /= D (C).Last_UTF8_Offset then
               raise Program_Error;
            end if;

            if J1.First_UTF16_Offset /= D (C).First_UTF16_Offset then
               raise Program_Error;
            end if;

            if J1.Last_UTF16_Offset /= D (C).Last_UTF16_Offset then
               raise Program_Error;
            end if;

            if not J2.Has_Element then
               raise Program_Error;
            end if;

            if J2.Character_Index /= C then
               raise Program_Error;
            end if;

            if J2.Character_Index not in D'Range then
               raise Program_Error;
            end if;

            if J2.Element /= D (C).Character then
               raise Program_Error;
            end if;

            if J2.First_UTF8_Offset /= D (C).First_UTF8_Offset then
               raise Program_Error;
            end if;

            if J2.Last_UTF8_Offset /= D (C).Last_UTF8_Offset then
               raise Program_Error;
            end if;

            if J2.First_UTF16_Offset /= D (C).First_UTF16_Offset then
               raise Program_Error;
            end if;

            if J2.Last_UTF16_Offset /= D (C).Last_UTF16_Offset then
               raise Program_Error;
            end if;
         end;

         C := C - 1;

         if not J.Backward then
            if J.Has_Element then
               raise Program_Error;
            end if;

            if C /= 0 then
               raise Program_Error;
            end if;

            exit;
         end if;
      end loop;
   end Test_Backward;

   ------------------
   -- Test_Forward --
   ------------------

   procedure Test_Forward is
      J : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.First_Character;
      C : VSS.Strings.Character_Index := 1;
      M : VSS.Strings.Cursors.Markers.Character_Marker;

   begin
      loop
         --  Check position of the cursor

         if not J.Has_Element then
            raise Program_Error;
         end if;

         if C /= J.Character_Index then
            raise Program_Error;
         end if;

         if J.Character_Index not in D'Range then
            raise Program_Error;
         end if;

         if J.Element /= D (C).Character then
            raise Program_Error;
         end if;

         if J.First_UTF8_Offset /= D (C).First_UTF8_Offset then
            raise Program_Error;
         end if;

         if J.Last_UTF8_Offset /= D (C).Last_UTF8_Offset then
            raise Program_Error;
         end if;

         if J.First_UTF16_Offset /= D (C).First_UTF16_Offset then
            raise Program_Error;
         end if;

         if J.Last_UTF16_Offset /= D (C).Last_UTF16_Offset then
            raise Program_Error;
         end if;

         --  Create mark and check its position

         M := J.Marker;

         if M.Character_Index /= C then
            raise Program_Error;
         end if;

         if M.Character_Index not in D'Range then
            raise Program_Error;
         end if;

         --  if M.Element /= D (C).Character then
         --     raise Program_Error;
         --  end if;

         --  GNAT 20210228: subprograms of Abstract_Cursor interface is not
         --  visible, thus implicit conversion is used.

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).First_UTF8_Offset
           /= D (C).First_UTF8_Offset
         then
            raise Program_Error;
         end if;

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).Last_UTF8_Offset
           /= D (C).Last_UTF8_Offset
         then
            raise Program_Error;
         end if;

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).First_UTF16_Offset
           /= D (C).First_UTF16_Offset
         then
            raise Program_Error;
         end if;

         if VSS.Strings.Cursors.Abstract_Cursor'Class (M).Last_UTF16_Offset
           /= D (C).Last_UTF16_Offset
         then
            raise Program_Error;
         end if;

         --  Create iterators from the iterator and from the mark and check
         --  their position

         declare
            J1 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
              S.Character (J);
            J2 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
              S.Character (M);

         begin
            if not J1.Has_Element then
               raise Program_Error;
            end if;

            if J1.Character_Index /= C then
               raise Program_Error;
            end if;

            if J1.Character_Index not in D'Range then
               raise Program_Error;
            end if;

            if J1.Element /= D (C).Character then
               raise Program_Error;
            end if;

            if J1.First_UTF8_Offset /= D (C).First_UTF8_Offset then
               raise Program_Error;
            end if;

            if J1.Last_UTF8_Offset /= D (C).Last_UTF8_Offset then
               raise Program_Error;
            end if;

            if J1.First_UTF16_Offset /= D (C).First_UTF16_Offset then
               raise Program_Error;
            end if;

            if J1.Last_UTF16_Offset /= D (C).Last_UTF16_Offset then
               raise Program_Error;
            end if;

            if not J2.Has_Element then
               raise Program_Error;
            end if;

            if J2.Character_Index /= C then
               raise Program_Error;
            end if;

            if J2.Character_Index not in D'Range then
               raise Program_Error;
            end if;

            if J2.Element /= D (C).Character then
               raise Program_Error;
            end if;

            if J2.First_UTF8_Offset /= D (C).First_UTF8_Offset then
               raise Program_Error;
            end if;

            if J2.Last_UTF8_Offset /= D (C).Last_UTF8_Offset then
               raise Program_Error;
            end if;

            if J2.First_UTF16_Offset /= D (C).First_UTF16_Offset then
               raise Program_Error;
            end if;

            if J2.Last_UTF16_Offset /= D (C).Last_UTF16_Offset then
               raise Program_Error;
            end if;
         end;

         C := C + 1;

         if not J.Forward then
            if J.Has_Element then
               raise Program_Error;
            end if;

            if C <= D'Last then
               raise Program_Error;
            end if;

            exit;
         end if;
      end loop;
   end Test_Forward;

begin
   Test_Forward;
   Test_Backward;
end Test_Character_Iterators;
