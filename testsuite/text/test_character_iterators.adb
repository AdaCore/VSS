--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Unicode;

with VSS.Strings.Cursors.Markers;

with Test_Support;

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
   procedure Test_Forward_From_Before_First;
   procedure Test_Backward;
   procedure Test_Backward_After_Last;

   procedure Test_V627_026_Empty_Segments;
   --  Test that last character index is equal to first character index when
   --  iterator is not initialized or point before the first character or
   --  after last character of the string data, and there is no exception
   --  raised.

   procedure Test_Common_Backward
     (J : in out VSS.Strings.Character_Iterators.Character_Iterator);

   procedure Test_Common_Forward
     (J : in out VSS.Strings.Character_Iterators.Character_Iterator);

   -------------------
   -- Test_Backward --
   -------------------

   procedure Test_Backward is
      J : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_Last_Character;

   begin
      Test_Common_Backward (J);
   end Test_Backward;

   ------------------------------
   -- Test_Backward_After_Last --
   ------------------------------

   procedure Test_Backward_After_Last is
      J : VSS.Strings.Character_Iterators.Character_Iterator;

   begin
      J.Set_After_Last (S);

      Test_Support.Assert (not J.Has_Element);

      Test_Support.Assert (J.Backward);

      Test_Common_Backward (J);
   end Test_Backward_After_Last;

   --------------------------
   -- Test_Common_Backward --
   --------------------------

   procedure Test_Common_Backward
     (J : in out VSS.Strings.Character_Iterators.Character_Iterator)
   is
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
              S.At_Character (J);
            J2 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
              S.At_Character (M);

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
   end Test_Common_Backward;

   -------------------------
   -- Test_Common_Forward --
   -------------------------

   procedure Test_Common_Forward
     (J : in out VSS.Strings.Character_Iterators.Character_Iterator)
   is
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
              S.At_Character (J);
            J2 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
              S.At_Character (M);

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
   end Test_Common_Forward;

   ------------------
   -- Test_Forward --
   ------------------

   procedure Test_Forward is
      J : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_First_Character;
   begin
      Test_Common_Forward (J);
   end Test_Forward;

   ------------------------------------
   -- Test_Forward_From_Before_First --
   ------------------------------------

   procedure Test_Forward_From_Before_First is
      J : VSS.Strings.Character_Iterators.Character_Iterator;

   begin
      J.Set_Before_First (S);

      Test_Support.Assert (not J.Has_Element);

      Test_Support.Assert (J.Forward);

      Test_Common_Forward (J);
   end Test_Forward_From_Before_First;

   ----------------------------------
   -- Test_V627_026_Empty_Segments --
   ----------------------------------

   procedure Test_V627_026_Empty_Segments is
      S : constant VSS.Strings.Virtual_String := "ABC";
      A : constant VSS.Strings.Character_Count := S.Character_Length + 1;

   begin
      declare
         JB : constant VSS.Strings.Character_Iterators.Character_Iterator :=
           S.Before_First_Character;
         JA : constant VSS.Strings.Character_Iterators.Character_Iterator :=
           S.After_Last_Character;
         JE : VSS.Strings.Character_Iterators.Character_Iterator;
         JF : VSS.Strings.Character_Iterators.Character_Iterator :=
           S.At_First_Character;
         JL : VSS.Strings.Character_Iterators.Character_Iterator :=
           S.At_Last_Character;

      begin
         Test_Support.Assert (JB.Character_Index = 0);
         Test_Support.Assert (JB.First_Character_Index = 0);
         Test_Support.Assert
           (JB.First_Character_Index = JB.Last_Character_Index);

         Test_Support.Assert (JA.Character_Index = A);
         Test_Support.Assert (JA.First_Character_Index = A);
         Test_Support.Assert
           (JA.First_Character_Index = JA.Last_Character_Index);

         Test_Support.Assert
           (JE.First_Character_Index = JE.Last_Character_Index);

         Test_Support.Assert (not JF.Backward);
         Test_Support.Assert (JF.Character_Index = 0);
         Test_Support.Assert (JF.First_Character_Index = 0);
         Test_Support.Assert
           (JF.First_Character_Index = JF.Last_Character_Index);

         Test_Support.Assert (not JL.Forward);
         Test_Support.Assert (JL.Character_Index = A);
         Test_Support.Assert (JL.First_Character_Index = A);
         Test_Support.Assert
           (JL.First_Character_Index = JL.Last_Character_Index);
      end;
   end Test_V627_026_Empty_Segments;

begin
   Test_Forward;
   Test_Forward_From_Before_First;
   Test_Backward;
   Test_Backward_After_Last;
   Test_V627_026_Empty_Segments;
end Test_Character_Iterators;
