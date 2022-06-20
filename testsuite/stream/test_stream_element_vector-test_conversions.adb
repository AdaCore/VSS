--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;

separate (Test_Stream_Element_Vector)

procedure Test_Conversions is
   U : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("ABC123");
   V : constant VSS.Stream_Element_Vectors.Stream_Element_Vector :=
     VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
       (U);
   S : constant String :=
     VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String (V);

begin
   Test_Support.Assert (S = Ada.Strings.Unbounded.To_String (U));
end Test_Conversions;
