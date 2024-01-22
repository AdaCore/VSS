--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;

with VSS.Command_Line.Parsers;
with VSS.String_Vectors;
with VSS.Strings;

with Test_Support;

procedure Test_Command_Line_Parser is

   use type Ada.Containers.Count_Type;
   use type VSS.Strings.Virtual_String;

   procedure Test_Short_Equal;
   --  Test of "-P=project.gpr"

   procedure Test_Short_Next;
   --  Test of "-P" "project.gpr"

   procedure Test_Short_No_Separator;
   --  Test of "-Pproject.gpr"

   procedure Test_Long_Equal;
   --  Test of "--project=project.gpr"

   procedure Test_Long_Next;
   --  Test of "--project" "project.gpr"

   procedure Test_Multiple_Values_Mixed;
   --  Test of "-o" "a" "-ob" "-o=c" "--output=d" "--output" "e"

   procedure Test_Name_Value_No_Separator;
   --  Test of "-Xname=value"

   procedure Test_Name_Value_Next;
   --  Test of "-X" "name=value"

   procedure Test_Name_Value_Mixed;
   --  Test of "-X" "name1=value1" "-Xname2=value2"

   procedure Test_Long_Binary;
   --  Test that "--subprojects" raise error

   procedure Test_Short_Binary;
   --  Test that "-S" raise error

   procedure Test_Long_Binary_With_Equal_Sign_Error;
   --  Test that "--subprojects=" raise error

   procedure Test_Long_Binary_With_Value_Error;
   --  Test that "--subprojects=project.gpr" raise error

   procedure Test_Short_Binary_With_Equal_Sign_Error;
   --  Test that "-S=" raise error

   procedure Test_Short_Binary_With_Value_Error;
   --  Test that "--S=project.gpr" raise error

   procedure Test_Few_Positionals_All_Specified;
   --  Test processing of the positional arguments.

   procedure Test_Few_Positionals_Long_List;
   --  Test processing of the positional arguments: list of arguments is
   --  longer than number of defined positional arguments.

   procedure Test_Positional_Unspecified;
   --  Test processing of the positional arguments: some of defined positional
   --  argument is not specified in the command line.

   procedure Test_Command_Line_Parser;
   --  Run testcases of Command_Line_Parser.

   procedure Test_Multiple_Positional;
   --  Test multiple positional option

   ------------------------------
   -- Test_Command_Line_Parser --
   ------------------------------

   procedure Test_Command_Line_Parser is
   begin
      Test_Support.Run_Testcase
        (Test_Short_No_Separator'Access,
         "short option with value without separator");
      Test_Support.Run_Testcase
        (Test_Short_Equal'Access,
         "short option with value after equal separator");
      Test_Support.Run_Testcase
        (Test_Short_Next'Access,
         "short option with value as following argument");
      Test_Support.Run_Testcase
        (Test_Long_Equal'Access,
         "long option with value after equal separator");
      Test_Support.Run_Testcase
        (Test_Long_Next'Access,
         "long option with value as following argument");
      Test_Support.Run_Testcase
        (Test_Multiple_Values_Mixed'Access,
         "multiple values of different styles");
      Test_Support.Run_Testcase
        (Test_Name_Value_No_Separator'Access,
         "name-value without separator");
      Test_Support.Run_Testcase
        (Test_Name_Value_Next'Access,
         "name-value as following argument");
      Test_Support.Run_Testcase
        (Test_Name_Value_Mixed'Access,
         "name-value with different styles");

      Test_Support.Run_Testcase
        (Test_Long_Binary'Access,
         "long boolean option");
      Test_Support.Run_Testcase
        (Test_Short_Binary'Access,
         "short boolean option");
      Test_Support.Run_Testcase
        (Test_Long_Binary_With_Equal_Sign_Error'Access,
         "long boolean option with equal separator no value");
      Test_Support.Run_Testcase
        (Test_Long_Binary_With_Value_Error'Access,
         "long boolean option with value after equal separator");
      Test_Support.Run_Testcase
        (Test_Short_Binary_With_Equal_Sign_Error'Access,
         "short boolean option with equal separator no value");
      Test_Support.Run_Testcase
        (Test_Short_Binary_With_Value_Error'Access,
         "short boolean option with value after equal separator");

      Test_Support.Run_Testcase
        (Test_Few_Positionals_All_Specified'Access,
         "few positional arguments all specified");
      Test_Support.Run_Testcase
        (Test_Few_Positionals_Long_List'Access,
         "few positional arguments more arguments");
      Test_Support.Run_Testcase
        (Test_Positional_Unspecified'Access,
         "single positional arguments without value specified");

      Test_Support.Run_Testcase
        (Test_Multiple_Positional'Access,
         "multiple positional argument");
   end Test_Command_Line_Parser;

   --------------------------
   -- Test_Few_Positionals --
   --------------------------

   procedure Test_Few_Positionals_All_Specified is
      use type VSS.String_Vectors.Virtual_String_Vector;

      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option_A  : constant VSS.Command_Line.Positional_Option :=
        (Name        => "file.a",
         Description => "");
      Option_B  : constant VSS.Command_Line.Positional_Option :=
        (Name        => "file.b",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option_A);
      Parser.Add_Option (Option_B);

      Arguments.Append ("file1");
      Arguments.Append ("file2");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Positional_Arguments = Arguments);
      Test_Support.Assert (Parser.Is_Specified (Option_A));
      Test_Support.Assert (Parser.Value (Option_A) = "file1");
      Test_Support.Assert (Parser.Is_Specified (Option_B));
      Test_Support.Assert (Parser.Value (Option_B) = "file2");
   end Test_Few_Positionals_All_Specified;

   ------------------------------------
   -- Test_Few_Positionals_Long_List --
   ------------------------------------

   procedure Test_Few_Positionals_Long_List is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option_A  : constant VSS.Command_Line.Positional_Option :=
        (Name        => "file.a",
         Description => "");
      Option_B  : constant VSS.Command_Line.Positional_Option :=
        (Name        => "file.b",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option_A);
      Parser.Add_Option (Option_B);

      Arguments.Append ("file1");
      Arguments.Append ("file2");
      Arguments.Append ("file3");

      Test_Support.Assert (not Parser.Parse (Arguments));
      Test_Support.Assert (not Parser.Error_Message.Is_Empty);
   end Test_Few_Positionals_Long_List;

   ----------------------
   -- Test_Long_Binary --
   ----------------------

   procedure Test_Long_Binary is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => "S",
         Long_Name   => "subprojects",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("--subprojects");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
   end Test_Long_Binary;

   --------------------------------------------
   -- Test_Long_Binary_With_Equal_Sign_Error --
   --------------------------------------------

   procedure Test_Long_Binary_With_Equal_Sign_Error is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => "S",
         Long_Name   => "subprojects",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("--subprojects=");

      Test_Support.Assert (not Parser.Parse (Arguments));
      Test_Support.Assert (not Parser.Error_Message.Is_Empty);
      Test_Support.Assert (not Parser.Is_Specified (Option));
   end Test_Long_Binary_With_Equal_Sign_Error;

   ---------------------------------------
   -- Test_Long_Binary_With_Value_Error --
   ---------------------------------------

   procedure Test_Long_Binary_With_Value_Error is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => "S",
         Long_Name   => "subprojects",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("--subprojects=project.gpr");

      Test_Support.Assert (not Parser.Parse (Arguments));
      Test_Support.Assert (not Parser.Error_Message.Is_Empty);
      Test_Support.Assert (not Parser.Is_Specified (Option));
   end Test_Long_Binary_With_Value_Error;

   ---------------------
   -- Test_Long_Equal --
   ---------------------

   procedure Test_Long_Equal is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Value_Option :=
        (Short_Name  => "P",
         Long_Name   => "project",
         Value_Name  => "file",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("--project=project.gpr");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Value (Option) = "project.gpr");
   end Test_Long_Equal;

   --------------------
   -- Test_Long_Next --
   --------------------

   procedure Test_Long_Next is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Value_Option :=
        (Short_Name  => "P",
         Long_Name   => "project",
         Value_Name  => "file",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("--project");
      Arguments.Append ("project.gpr");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Value (Option) = "project.gpr");
   end Test_Long_Next;

   ------------------------------
   -- Test_Multiple_Positional --
   ------------------------------

   procedure Test_Multiple_Positional is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Multivalue_Positional_Option :=
        (Name        => "files",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("file1");
      Arguments.Append ("file2");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Values (Option) (1) = "file1");
      Test_Support.Assert (Parser.Values (Option) (2) = "file2");
   end Test_Multiple_Positional;

   --------------------------------
   -- Test_Multiple_Values_Mixed --
   --------------------------------

   procedure Test_Multiple_Values_Mixed is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Value_Option :=
        (Short_Name  => "o",
         Long_Name   => "output",
         Value_Name  => "value",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-o");
      Arguments.Append ("a");
      Arguments.Append ("-ob");
      Arguments.Append ("-o=c");
      Arguments.Append ("--output=d");
      Arguments.Append ("--output");
      Arguments.Append ("e");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Values (Option).Length = 5);
      Test_Support.Assert (Parser.Values (Option) (1) = "a");
      Test_Support.Assert (Parser.Values (Option) (2) = "b");
      Test_Support.Assert (Parser.Values (Option) (3) = "c");
      Test_Support.Assert (Parser.Values (Option) (4) = "d");
      Test_Support.Assert (Parser.Values (Option) (5) = "e");
   end Test_Multiple_Values_Mixed;

   ---------------------------
   -- Test_Name_Value_Mixed --
   ---------------------------

   procedure Test_Name_Value_Mixed is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Name_Value_Option :=
        (Short_Name  => "X",
         Long_Name   => "",
         Name_Name   => "name",
         Value_Name  => "value",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-X");
      Arguments.Append ("name1=value1");
      Arguments.Append ("-Xname2=value2");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Values (Option).Length = 2);
      Test_Support.Assert (Parser.Values (Option) (1).Name = "name1");
      Test_Support.Assert (Parser.Values (Option) (1).Value = "value1");
      Test_Support.Assert (Parser.Values (Option) (2).Name = "name2");
      Test_Support.Assert (Parser.Values (Option) (2).Value = "value2");
   end Test_Name_Value_Mixed;

   --------------------------
   -- Test_Name_Value_Next --
   --------------------------

   procedure Test_Name_Value_Next is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Name_Value_Option :=
        (Short_Name  => "X",
         Long_Name   => "",
         Name_Name   => "name",
         Value_Name  => "value",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-X");
      Arguments.Append ("name=value");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Values (Option).Length = 1);
      Test_Support.Assert (Parser.Values (Option) (1).Name = "name");
      Test_Support.Assert (Parser.Values (Option) (1).Value = "value");
   end Test_Name_Value_Next;

   ----------------------------------
   -- Test_Name_Value_No_Separator --
   ----------------------------------

   procedure Test_Name_Value_No_Separator is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Name_Value_Option :=
        (Short_Name  => "X",
         Long_Name   => "",
         Name_Name   => "name",
         Value_Name  => "value",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-Xname=value");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Values (Option).Length = 1);
      Test_Support.Assert (Parser.Values (Option) (1).Name = "name");
      Test_Support.Assert (Parser.Values (Option) (1).Value = "value");
   end Test_Name_Value_No_Separator;

   ---------------------------------
   -- Test_Positional_Unspecified --
   ---------------------------------

   procedure Test_Positional_Unspecified is
      use type VSS.String_Vectors.Virtual_String_Vector;

      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option_A  : constant VSS.Command_Line.Positional_Option :=
        (Name        => "file.a",
         Description => "");
      Option_B  : constant VSS.Command_Line.Positional_Option :=
        (Name        => "file.b",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option_A);
      Parser.Add_Option (Option_B);

      Arguments.Append ("file1");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Positional_Arguments = Arguments);
      Test_Support.Assert (Parser.Is_Specified (Option_A));
      Test_Support.Assert (Parser.Value (Option_A) = "file1");
      Test_Support.Assert (not Parser.Is_Specified (Option_B));
      Test_Support.Assert (Parser.Value (Option_B).Is_Empty);
   end Test_Positional_Unspecified;

   -----------------------
   -- Test_Short_Binary --
   -----------------------

   procedure Test_Short_Binary is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => "S",
         Long_Name   => "subprojects",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-S");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
   end Test_Short_Binary;

   ---------------------------------------------
   -- Test_Short_Binary_With_Equal_Sign_Error --
   ---------------------------------------------

   procedure Test_Short_Binary_With_Equal_Sign_Error is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => "S",
         Long_Name   => "subprojects",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-S=");

      Test_Support.Assert (not Parser.Parse (Arguments));
      Test_Support.Assert (not Parser.Error_Message.Is_Empty);
      Test_Support.Assert (not Parser.Is_Specified (Option));
   end Test_Short_Binary_With_Equal_Sign_Error;

   ----------------------------------------
   -- Test_Short_Binary_With_Value_Error --
   ----------------------------------------

   procedure Test_Short_Binary_With_Value_Error is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Binary_Option :=
        (Short_Name  => "S",
         Long_Name   => "subprojects",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-S=project.gpr");

      Test_Support.Assert (not Parser.Parse (Arguments));
      Test_Support.Assert (not Parser.Error_Message.Is_Empty);
      Test_Support.Assert (not Parser.Is_Specified (Option));
   end Test_Short_Binary_With_Value_Error;

   ----------------------
   -- Test_Short_Equal --
   ----------------------

   procedure Test_Short_Equal is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Value_Option :=
        (Short_Name  => "P",
         Long_Name   => "project",
         Value_Name  => "file",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-P=project.gpr");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Value (Option) = "project.gpr");
   end Test_Short_Equal;

   ---------------------
   -- Test_Short_Next --
   ---------------------

   procedure Test_Short_Next is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Value_Option :=
        (Short_Name  => "P",
         Long_Name   => "project",
         Value_Name  => "file",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-P");
      Arguments.Append ("project.gpr");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Value (Option) = "project.gpr");
   end Test_Short_Next;

   -----------------------------
   -- Test_Short_No_Separator --
   -----------------------------

   procedure Test_Short_No_Separator is
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Option    : constant VSS.Command_Line.Value_Option :=
        (Short_Name  => "P",
         Long_Name   => "project",
         Value_Name  => "file",
         Description => "");
      Parser    : VSS.Command_Line.Parsers.Command_Line_Parser;

   begin
      Parser.Add_Option (Option);

      Arguments.Append ("-Pproject.gpr");

      Test_Support.Assert (Parser.Parse (Arguments));
      Test_Support.Assert (Parser.Error_Message.Is_Empty);
      Test_Support.Assert (Parser.Is_Specified (Option));
      Test_Support.Assert (Parser.Value (Option) = "project.gpr");
   end Test_Short_No_Separator;

begin
   Test_Support.Run_Testsuite
     (Test_Command_Line_Parser'Access, "Command Line Parser");
end Test_Command_Line_Parser;
