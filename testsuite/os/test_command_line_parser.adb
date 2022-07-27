--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
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

   procedure Test_Name_Value_No_Separator;
   --  Test of "-Xname=value"

   procedure Test_Name_Value_Next;
   --  Test of "-X" "name=value"

   procedure Test_Name_Value_Mixed;
   --  Test of "-X" "name1=value1" "-Xname2=value2"

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
   Test_Short_No_Separator;
   Test_Short_Equal;
   Test_Short_Next;
   Test_Long_Equal;
   Test_Long_Next;
   Test_Name_Value_No_Separator;
   Test_Name_Value_Next;
   Test_Name_Value_Mixed;
end Test_Command_Line_Parser;
