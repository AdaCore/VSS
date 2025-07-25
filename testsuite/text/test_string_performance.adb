--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with VSS.String_Vectors;
with VSS.Strings.Conversions;
with VSS.Utils.File_IO;

with Test_Support;

procedure Test_String_Performance is

   procedure Test_String;

   procedure Test_Is_Equal;

   -------------------
   -- Test_Is_Equal --
   -------------------

   procedure Test_Is_Equal is

      use type Ada.Calendar.Time;

      type Unbounded_String_Array is
        array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

      type Unbounded_String_Array_Access is access all Unbounded_String_Array;

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Unbounded_String_Array, Unbounded_String_Array_Access);

      type Virtual_String_Array is
        array (Positive range <>) of VSS.Strings.Virtual_String;

      type Virtual_String_Array_Access is access all Virtual_String_Array;

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Virtual_String_Array, Virtual_String_Array_Access);

      type Relative_Performance is delta 0.01 digits 3 range 0.00 .. 9.99;

      function Compute
        (US : Duration; VS : Duration) return Relative_Performance;

      -------------
      -- Compute --
      -------------

      function Compute
        (US : Duration; VS : Duration) return Relative_Performance is
      begin
         return Relative_Performance (VS / US);
      end Compute;

      VSV : constant VSS.String_Vectors.Virtual_String_Vector :=
        VSS.Utils.File_IO.Load
          ("testsuite/text/performance/src_editor_buffer.adb",
           "utf-8").Split_Lines;

      AUS : Unbounded_String_Array_Access :=
        new Unbounded_String_Array (1 .. VSV.Length);
      AVS : Virtual_String_Array_Access :=
        new Virtual_String_Array (1 .. VSV.Length);

      USC : Natural := 0;
      VSC : Natural := 0;
      UST : Duration;
      VST : Duration;
      P   : Relative_Performance;

   begin
      --  Fill arrays

      for J in 1 .. VSV.Length loop
         AVS (J) := VSV (J);
         AUS (J) :=
           VSS.Strings.Conversions.To_Unbounded_UTF_8_String (VSV (J));
      end loop;

      --  array <Unbounded_String>

      declare
         use type Ada.Strings.Unbounded.Unbounded_String;

         Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      begin
         for J in AUS'Range loop
            for K in AUS'Range loop
               USC := @ + (if AUS (J) = AUS (K) then 1 else 0);
            end loop;
         end loop;

         UST := Ada.Calendar.Clock - Start;
      end;

      --  array <Virtual_String>

      declare
         use type VSS.Strings.Virtual_String;

         Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      begin
         for J in AVS'Range loop
            for K in AVS'Range loop
               VSC := @ + (if AVS (J) = AVS (K) then 1 else 0);
            end loop;
         end loop;

         VST := Ada.Calendar.Clock - Start;
      end;

      P := Compute (UST, VST);

      Test_Support.Assert (USC = VSC);
      Test_Support.Assert
        (P <= 0.66,
         "performance" & Relative_Performance'Image (P) & " is too bad");
      Test_Support.Assert
        (0.51 <= P,
         "performance" & Relative_Performance'Image (P) & " is too good");

      Free (AUS);
      Free (AVS);

   exception
      when others =>
         Free (AUS);
         Free (AVS);

         raise;
   end Test_Is_Equal;

   -----------------
   -- Test_String --
   -----------------

   procedure Test_String is
   begin
      Test_Support.Run_Testcase (Test_Is_Equal'Access, """="" performance");
   end Test_String;

begin
   Test_Support.Run_Testsuite
     (Test_String'Access, "Virtual_String performance");
end Test_String_Performance;
