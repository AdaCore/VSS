--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;

with VSS.String_Vectors;
with VSS.Strings.Word_Iterators;

with Test_Support;
with Generic_UCD_Break_Test_Runner;

procedure Test_Word_Iterators is

   procedure Run_Test_Case
     (String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector);

   procedure Run_UCD_Tests is
     new Generic_UCD_Break_Test_Runner (Run_Test_Case);

   -------------------
   -- Run_Test_Case --
   -------------------

   procedure Run_Test_Case
     (String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;

      JF : VSS.Strings.Word_Iterators.Word_Iterator := String.At_First_Word;
      --  JB : VSS.Strings.Word_Iterators.Word_Iterator := String.Last_Word;
      S  : Positive := 1;

   begin
      if String.Is_Empty then
         Test_Support.Assert (not JF.Has_Element);
         --  Test_Support.Assert (not JB.Has_Element);

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

      --  loop
      --     Test_Support.Assert (JB.Has_Element);
      --
      --     S := S - 1;
      --     Test_Support.Assert (String.Slice (JB) = Segments (S));
      --
      --     exit when not JB.Backward;
      --  end loop;
      --
      --  Test_Support.Assert (S = 1);
      --  Test_Support.Assert (not JB.Has_Element);
   end Run_Test_Case;

begin
   --  Process test cases provided with UCD.

   Run_UCD_Tests
     (Ada.Command_Line.Argument (1) & "/auxiliary/WordBreakTest.txt");

   --  Additional test for an empty string.

   Run_Test_Case
     (VSS.Strings.Empty_Virtual_String,
      VSS.String_Vectors.Empty_Virtual_String_Vector);
end Test_Word_Iterators;
