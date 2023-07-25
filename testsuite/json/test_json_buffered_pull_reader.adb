--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.JSON.Pull_Readers.Buffered;
with VSS.JSON.Streams;

with Tests_JSON_Streams;

procedure Test_JSON_Buffered_Pull_Reader is

   use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

   Data :
     aliased constant Tests_JSON_Streams.JSON_Stream_Element_Vectors.Vector :=
       [(Kind => Start_Document),
        (Kind => Start_Object),
        (Kind => Key_Name, Key_Name => "member"),
        (Kind => Start_Array),
        (Kind => Null_Value),
        (Kind => Boolean_Value, Boolean_Value => True),
        (Kind         => Number_Value,
         Number_Value =>
           (Kind          => VSS.JSON.JSON_Integer,
            String_Value  => "123",
            Integer_Value => 123)),
        (Kind => String_Value, String_Value => "text"),
        (Kind => End_Array),
        (Kind => End_Object),
        (Kind => End_Document)];

   procedure Test_Mark_Reset;

   procedure Test_Mark_Reset is separate;

begin
   Test_Mark_Reset;
end Test_JSON_Buffered_Pull_Reader;
