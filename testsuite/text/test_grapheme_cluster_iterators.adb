--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;
with GNAT.Source_Info;

with VSS.Application;
with VSS.String_Vectors;
with VSS.Strings.Grapheme_Cluster_Iterators;

with Test_Support;
with Generic_UCD_Break_Test_Runner;

procedure Test_Grapheme_Cluster_Iterators is

   procedure Grapheme_Cluster_Iterator_Testsuite;
   --  Run testcases of the grapheme cluster iterator.

   procedure Run_Test_Case
     (String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector);

   procedure Test_UCD_GraphemeBreakTest;
   --  Run testcases from UCD's GraphemeBreakTest.txt file

   procedure Test_Empty_String;
   --  Test grapheme iterator on empty string

   procedure Test_V627_026_Empty_Segments;
   --  Test index of first and last characters in the segment when iterator is
   --  not initialized or points before the first character or after the last
   --  character of the string data.

   procedure Test_UCD_Emoji_Test;
   --  Test grapheme cluster iterator on cases provided by emoji-test.txt
   --  file. It checks that each emoji sequence occupies single grapheme
   --  cluser and detected as emoji.

   procedure Test_Display_Width;
   --  Test cell width computation.

   Emoji_Root : constant VSS.Strings.Virtual_String :=
     VSS.Application.Arguments.Element (2);

   -----------------------------------------
   -- Grapheme_Cluster_Iterator_Testsuite --
   -----------------------------------------

   procedure Grapheme_Cluster_Iterator_Testsuite is
   begin
      Test_Support.Run_Testcase
        (Test_UCD_GraphemeBreakTest'Access, "UCD GraphemeBreakTest.txt");

      Test_Support.Run_Testcase
        (Test_Empty_String'Access, "grapheme clusters of empty string");

      Test_Support.Run_Testcase
        (Test_V627_026_Empty_Segments'Access,
         "V627-026 indicies of the uninitialized iterator");

      Test_Support.Run_Testcase
        (Test_UCD_Emoji_Test'Access, "UCD emoji-test.txt");

      Test_Support.Run_Testcase
        (Test_Display_Width'Access, "display cell width");
   end Grapheme_Cluster_Iterator_Testsuite;

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

   ------------------------
   -- Test_Display_Width --
   ------------------------

   procedure Test_Display_Width is

      use type VSS.Strings.Display_Cell_Count;

      procedure Simple_Test
        (Item     : VSS.Strings.Virtual_String;
         Width    : VSS.Strings.Display_Cell_Count;
         Message  : String;
         Location : String := GNAT.Source_Info.Source_Location);

      -----------------
      -- Simple_Test --
      -----------------

      procedure Simple_Test
        (Item     : VSS.Strings.Virtual_String;
         Width    : VSS.Strings.Display_Cell_Count;
         Message  : String;
         Location : String := GNAT.Source_Info.Source_Location)
      is
         Iterator : VSS.Strings.Grapheme_Cluster_Iterators
                      .Grapheme_Cluster_Iterator :=
           Item.Before_First_Grapheme_Cluster;

      begin
         Test_Support.Assert
           (Iterator.Display_Width = 0, Message & " at " & Location);
         Test_Support.Assert (Iterator.Forward, Message & " at " & Location);
         Test_Support.Assert
           (Iterator.Display_Width = Width, Message & " at " & Location);
         Test_Support.Assert
           (not Iterator.Forward, Message & " at " & Location);
         Test_Support.Assert
           (Iterator.Display_Width = 0, Message & " at " & Location);
      end Simple_Test;

   begin
      Simple_Test (" ", 1, "space character");
      Simple_Test ("#", 1, "emoji with default text representation");
      Simple_Test ("丐", 2, "wide character");
      Simple_Test ("👩‍🔬", 2, "emoji sequence");

      --  Test of the "null" string

      declare
         Item     : VSS.Strings.Virtual_String;
         Iterator : constant VSS.Strings.Grapheme_Cluster_Iterators
                      .Grapheme_Cluster_Iterator :=
           Item.Before_First_Grapheme_Cluster;

      begin
         Test_Support.Assert (Iterator.Display_Width = 0);
      end;

      --  Test of the empty string

      declare
         Item     : constant VSS.Strings.Virtual_String := "";
         Iterator : constant VSS.Strings.Grapheme_Cluster_Iterators
                      .Grapheme_Cluster_Iterator :=
           Item.Before_First_Grapheme_Cluster;

      begin
         Test_Support.Assert (Iterator.Display_Width = 0);
      end;

      --  Test of the uninitialized iterator

      declare
         Iterator : VSS.Strings.Grapheme_Cluster_Iterators
                      .Grapheme_Cluster_Iterator;

      begin
         Test_Support.Assert (Iterator.Display_Width = 0);
      end;
   end Test_Display_Width;

   -----------------------
   -- Test_Empty_String --
   -----------------------

   procedure Test_Empty_String is
   begin
      Run_Test_Case
        (VSS.Strings.Empty_Virtual_String,
         VSS.String_Vectors.Empty_Virtual_String_Vector);
   end Test_Empty_String;

   -------------------------
   -- Test_UCD_Emoji_Test --
   -------------------------

   procedure Test_UCD_Emoji_Test is separate;

   --------------------------------
   -- Test_UCD_GraphemeBreakTest --
   --------------------------------

   procedure Test_UCD_GraphemeBreakTest is
      procedure Run_UCD_Tests is
        new Generic_UCD_Break_Test_Runner (Run_Test_Case);

   begin
      Run_UCD_Tests
        (Ada.Command_Line.Argument (1) & "/auxiliary/GraphemeBreakTest.txt");
   end Test_UCD_GraphemeBreakTest;

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
   Test_Support.Run_Testsuite
     (Grapheme_Cluster_Iterator_Testsuite'Access, "Grapheme cluster iterator");
end Test_Grapheme_Cluster_Iterators;
