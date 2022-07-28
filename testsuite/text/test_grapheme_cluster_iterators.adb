--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;

with VSS.String_Vectors;
with VSS.Strings.Grapheme_Cluster_Iterators;

with Test_Support;
with Generic_UCD_Break_Test_Runner;

procedure Test_Grapheme_Cluster_Iterators is

   procedure Run_Test_Case
     (String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector);

   procedure Run_UCD_Tests is
     new Generic_UCD_Break_Test_Runner (Run_Test_Case);

   procedure Test_V627_026_Empty_Segments;
   --  Test index of first and last characters in the segment when iterator is
   --  not initialized or points before the first character or after the last
   --  character of the string data.

   -------------------
   -- Run_Test_Case --
   -------------------

   procedure Run_Test_Case
     (String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;

      JF : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator :=
        String.At_First_Grapheme_Cluster;
      JB : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator :=
        String.At_Last_Grapheme_Cluster;
      S  : Positive := 1;

   begin
      if String.Is_Empty then
         Test_Support.Assert (not JF.Has_Element);
         Test_Support.Assert (not JB.Has_Element);

         return;
      end if;

      loop
         Test_Support.Assert (JF.Has_Element);
         --  Test_Support.Assert (J.Element = Data.Segments (S));
         Test_Support.Assert (String.Slice (JF) = Segments (S));
         S := S + 1;

         exit when not JF.Forward;
      end loop;

      Test_Support.Assert (S - 1 = Segments.Length);
      Test_Support.Assert (not JF.Has_Element);

      loop
         Test_Support.Assert (JB.Has_Element);

         S := S - 1;
         Test_Support.Assert (String.Slice (JB) = Segments (S));

         exit when not JB.Backward;
      end loop;

      Test_Support.Assert (S = 1);
      Test_Support.Assert (not JB.Has_Element);
   end Run_Test_Case;

   ----------------------------------
   -- Test_V627_026_Empty_Segments --
   ----------------------------------

   procedure Test_V627_026_Empty_Segments is

      use type VSS.Strings.Character_Count;

      S : constant VSS.Strings.Virtual_String := "ABC";
      A : constant VSS.Strings.Character_Count := S.Character_Length + 1;

      JE : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator;
      JF : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator :=
        S.At_First_Grapheme_Cluster;
      JL : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator :=
        S.At_Last_Grapheme_Cluster;

   begin
      Test_Support.Assert (JE.First_Character_Index = 0);
      Test_Support.Assert
        (JE.First_Character_Index = JE.Last_Character_Index);

      Test_Support.Assert (not JF.Backward);
      Test_Support.Assert (JF.First_Character_Index = 0);
      Test_Support.Assert
        (JF.First_Character_Index = JF.Last_Character_Index);

      Test_Support.Assert (not JL.Forward);
      Test_Support.Assert (JL.First_Character_Index = A);
      Test_Support.Assert
        (JL.First_Character_Index > JL.Last_Character_Index);
   end Test_V627_026_Empty_Segments;

begin
   --  Process test cases provided with UCD.

   Run_UCD_Tests
     (Ada.Command_Line.Argument (1) & "/auxiliary/GraphemeBreakTest.txt");

   --  Additional test for an empty string.

   Run_Test_Case
     (VSS.Strings.Empty_Virtual_String,
      VSS.String_Vectors.Empty_Virtual_String_Vector);

   Test_V627_026_Empty_Segments;
end Test_Grapheme_Cluster_Iterators;
