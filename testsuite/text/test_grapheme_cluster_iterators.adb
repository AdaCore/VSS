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

   -------------------
   -- Run_Test_Case --
   -------------------

   procedure Run_Test_Case
     (String   : VSS.Strings.Virtual_String;
      Segments : VSS.String_Vectors.Virtual_String_Vector)
   is
      use type VSS.Strings.Virtual_String;

      JF : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator :=
        String.First_Grapheme_Cluster;
      JB : VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator :=
        String.Last_Grapheme_Cluster;
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

begin
   --  Process test cases provided with UCD.

   Run_UCD_Tests
     (Ada.Command_Line.Argument (1) & "/auxiliary/GraphemeBreakTest.txt");

   --  Additional test for an empty string.

   Run_Test_Case
     (VSS.Strings.Empty_Virtual_String,
      VSS.String_Vectors.Empty_Virtual_String_Vector);
end Test_Grapheme_Cluster_Iterators;
