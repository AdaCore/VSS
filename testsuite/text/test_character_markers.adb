--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Markers;

with Test_Support;

procedure Test_Character_Markers is
   use type VSS.Strings.Character_Count;

   procedure Test_Append;
   procedure Test_Insert;
   procedure Test_Vectors_Finalization;

   -----------------
   -- Test_Append --
   -----------------

   procedure Test_Append is
      S1  : VSS.Strings.Virtual_String := "Hello, ";
      M11 : constant VSS.Strings.Markers.Character_Marker :=
        S1.At_First_Character.Marker;
      M12 : constant VSS.Strings.Markers.Character_Marker :=
        S1.At_Last_Character.Marker;

   begin
      S1.Append ("world");

      Test_Support.Assert (M11.Character_Index = 1);
      Test_Support.Assert (M12.Character_Index = 7);

      S1.Append ('!');

      Test_Support.Assert (M11.Character_Index = 1);
      Test_Support.Assert (M12.Character_Index = 7);
   end Test_Append;

   -----------------
   -- Test_Insert --
   -----------------

   procedure Test_Insert is
      S1  : VSS.Strings.Virtual_String := "AC";
      J1  : VSS.Strings.Character_Iterators.Character_Iterator :=
        S1.At_First_Character;
      M11 : constant VSS.Strings.Markers.Character_Marker :=
        S1.At_First_Character.Marker;
      M12 : constant VSS.Strings.Markers.Character_Marker :=
        S1.At_Last_Character.Marker;

      S2  : VSS.Strings.Virtual_String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      J2  : VSS.Strings.Character_Iterators.Character_Iterator :=
        S2.At_First_Character;
      M21 : constant VSS.Strings.Markers.Character_Marker :=
        S2.At_First_Character.Marker;
      M22 : constant VSS.Strings.Markers.Character_Marker :=
        S2.At_Last_Character.Marker;

   begin
      Test_Support.Assert (J1.Forward);
      Test_Support.Assert (M11.Character_Index = 1);
      Test_Support.Assert (M12.Character_Index = 2);

      S1.Insert (J1, 'B');

      Test_Support.Assert (M11.Character_Index = 1);
      Test_Support.Assert (M12.Character_Index = 3);
      Test_Support.Assert (J2.Forward);
      Test_Support.Assert (J2.Forward);
      Test_Support.Assert (M21.Character_Index = 1);
      Test_Support.Assert (M22.Character_Index = 26);

      S2.Insert (J2, '1');

      Test_Support.Assert (M21.Character_Index = 1);
      Test_Support.Assert (M22.Character_Index = 27);
   end Test_Insert;

   -------------------------------
   -- Test_Vectors_Finalization --
   -------------------------------

   procedure Test_Vectors_Finalization is

      package Marker_Vectors is new Ada.Containers.Vectors
        (Natural,
         VSS.Strings.Markers.Character_Marker,
         VSS.Strings.Markers."=");

      S  : constant VSS.Strings.Virtual_String := "Hello, world!";
      J1 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_First_Character with Unreferenced;
      J2 : constant VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_Last_Character with Unreferenced;

   begin
      --  Test finalization of the markers inside the vector container.

      declare
         V : Marker_Vectors.Vector;

      begin
         V.Append (S.At_First_Character.Marker);
         V.Append (S.At_Last_Character.Marker);
      end;
   end Test_Vectors_Finalization;

begin
   Test_Append;
   Test_Insert;
   Test_Vectors_Finalization;
end Test_Character_Markers;
