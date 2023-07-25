--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Test_Support;

separate (Test_JSON_Buffered_Pull_Reader)
procedure Test_Mark_Reset is

   use type VSS.JSON.Streams.JSON_Stream_Element;

   procedure Test_Nonmarked;

   procedure Test_First_Marked;

   procedure Test_Middle_Marked;

   procedure Test_Middle_Middle_Marked;

   -----------------------
   -- Test_First_Marked --
   -----------------------

   procedure Test_First_Marked is
      R : aliased Tests_JSON_Streams.Replay_Pull_Reader (Data'Access);
      B : VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
            (R'Unchecked_Access);

   begin
      R.Initialize;

      B.Mark;
      Test_Support.Assert (not B.At_End);

      for J in Data.First_Index .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);

      --  Reset position and reread data.

      B.Reset;

      for J in Data.First_Index .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);
   end Test_First_Marked;

   ------------------------
   -- Test_Middle_Marked --
   ------------------------

   procedure Test_Middle_Marked is
      R : aliased Tests_JSON_Streams.Replay_Pull_Reader (Data'Access);
      B : VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
            (R'Unchecked_Access);
      N : constant Positive := (Data.Last_Index - Data.First_Index) / 2;

   begin
      R.Initialize;

      Test_Support.Assert (not B.At_End);

      for J in Data.First_Index .. Data.First_Index + N loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      B.Mark;

      for J in Data.First_Index + N + 1 .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);

      --  Reset position and reread data

      B.Reset;

      for J in Data.First_Index + N + 1 .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);
   end Test_Middle_Marked;

   -------------------------------
   -- Test_Middle_Middle_Marked --
   -------------------------------

   procedure Test_Middle_Middle_Marked is
      R : aliased Tests_JSON_Streams.Replay_Pull_Reader (Data'Access);
      B : VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
            (R'Unchecked_Access);
      N : constant Positive := (Data.Last_Index - Data.First_Index) / 2;
      M : constant Positive := N + N / 2;

   begin
      Test_Support.Assert (N < M);

      R.Initialize;

      Test_Support.Assert (not B.At_End);

      for J in Data.First_Index .. Data.First_Index + N loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      B.Mark;

      for J in Data.First_Index + N + 1 .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);

      --  Reset position and reread data

      B.Reset;

      for J in Data.First_Index + N + 1 .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);

      --  Reset position and read data till next mark point

      B.Reset;

      for J in Data.First_Index + N + 1 .. Data.First_Index + M loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      B.Mark;

      for J in Data.First_Index + M + 1 .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);

      B.Reset;

      for J in Data.First_Index + M + 1 .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);
   end Test_Middle_Middle_Marked;

   --------------------
   -- Test_Nonmarked --
   --------------------

   procedure Test_Nonmarked is
      R : aliased Tests_JSON_Streams.Replay_Pull_Reader (Data'Access);
      B : VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
            (R'Unchecked_Access);

   begin
      R.Initialize;

      Test_Support.Assert (not B.At_End);

      for J in Data.First_Index .. Data.Last_Index loop
         Test_Support.Assert (Data (J) = B.Element);
         Test_Support.Assert (not B.At_End);
         B.Read_Next;
      end loop;

      Test_Support.Assert (B.At_End);
   end Test_Nonmarked;

begin
   Test_Nonmarked;
   Test_First_Marked;
   Test_Middle_Marked;
   Test_Middle_Middle_Marked;
end Test_Mark_Reset;
