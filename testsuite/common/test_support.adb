--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Exception_Actions;

package body Test_Support is

   use type Ada.Strings.Unbounded.Unbounded_String;

   Default_Testsuite : constant String := "DEFAULT_TESTSUITE";
   Default_Testcase  : constant String := "DEFAULT_TESTCASE";

   type Testcase_Status is (Unknown, Succeed, Failed, Errored, Skipped);

   type Testcase_Information is record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Status     : Testcase_Status := Unknown;
      Message    : Ada.Strings.Unbounded.Unbounded_String;
      Traceback  : Ada.Strings.Unbounded.Unbounded_String;
      Assertions : Natural := 0;
   end record;

   package Testcase_Vectors is
     new Ada.Containers.Vectors (Positive, Testcase_Information);

   type Testsuite_Information is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Testcases : Testcase_Vectors.Vector;
   end record;

   package Testsuite_Vectors is
     new Ada.Containers.Vectors (Positive, Testsuite_Information);

   type Testsuite_Set_Information is record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Testsuites : Testsuite_Vectors.Vector;
   end record;

   type Test_Information is
     limited new Ada.Finalization.Limited_Controlled with record
      Testsuite_Set    : Testsuite_Set_Information;
      Active_Testsuite : Testsuite_Information;
      Active_Testcase  : Testcase_Information;
   end record;

   overriding procedure Finalize (Self : in out Test_Information);

   procedure Global_Unhandled_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   Controller : Test_Information;

   procedure Start_Testsuite (Name : String);

   procedure End_Testsuite;

   procedure Start_Testcase (Name : String);

   procedure End_Testcase;

   procedure Write_JUnit_XML (File : String);

   function Escape_Attribute_Value
     (Item : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Condition : Boolean;
      Message   : String := "";
      Location  : String := GNAT.Source_Info.Source_Location) is
   begin
      if Controller.Active_Testcase.Name = "" then
         --  Start default testcase.

         Start_Testcase (Default_Testcase);
      end if;

      --  Increment assertions count

      Controller.Active_Testcase.Assertions := @ + 1;

      if not Condition then
         Fail (Message, Location);
      end if;
   end Assert;

   ------------------
   -- End_Testcase --
   ------------------

   procedure End_Testcase is
   begin
      Controller.Active_Testsuite.Testcases.Append
        (Controller.Active_Testcase);

      Controller.Active_Testcase :=
        (Name       => <>,
         Status     => Unknown,
         Message    => <>,
         Traceback  => <>,
         Assertions => 0);
   end End_Testcase;

   -------------------
   -- End_Testsuite --
   -------------------

   procedure End_Testsuite is
   begin
      Controller.Testsuite_Set.Testsuites.Append (Controller.Active_Testsuite);
      Controller.Active_Testsuite := (Name => <>, Testcases => <>);
   end End_Testsuite;

   ----------------------------
   -- Escape_Attribute_Value --
   ----------------------------

   function Escape_Attribute_Value
     (Item : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Result : Ada.Strings.Unbounded.Unbounded_String do
         for J in 1 .. Ada.Strings.Unbounded.Length (Item) loop
            case Ada.Strings.Unbounded.Element (Item, J) is
               when '&' =>
                  Ada.Strings.Unbounded.Append (Result, "&amp;");

               when others =>
                  Ada.Strings.Unbounded.Append
                    (Result, Ada.Strings.Unbounded.Element (Item, J));
            end case;
         end loop;
      end return;
   end Escape_Attribute_Value;

   ----------
   -- Fail --
   ----------

   procedure Fail
     (Message  : String := "";
      Location : String := GNAT.Source_Info.Source_Location) is
   begin
      if Controller.Active_Testcase.Name = "" then
         --  Start default testcase.

         Start_Testcase (Default_Testcase);
      end if;

      Controller.Active_Testcase.Message :=
        Ada.Strings.Unbounded.To_Unbounded_String (Message);

      Controller.Active_Testcase.Traceback :=
        Ada.Strings.Unbounded.To_Unbounded_String (Location);

      raise Test_Failed with "at "
              & Location
              & (if Message /= "" then " " & Message else "");
   end Fail;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Test_Information) is
      JUnit_XML_Variable : constant String := "XUNIT_XML_PATH";

      Verbose            : constant Boolean :=
        (for some J in 1 .. Ada.Command_Line.Argument_Count =>
           Ada.Command_Line.Argument (J) = "--verbose")
        or else
          Ada.Environment_Variables.Exists ("VERBOSE_TEST_REPORT");

   begin
      if Controller.Active_Testcase.Name = Default_Testcase then
         --  End default testcase.

         End_Testcase;
      end if;

      if Controller.Active_Testsuite.Name = Default_Testsuite then
         --  End default testsuite.

         End_Testsuite;
      end if;

      if Ada.Environment_Variables.Exists (JUnit_XML_Variable) then
         declare
            Path : constant String :=
              Ada.Environment_Variables.Value (JUnit_XML_Variable);

            Main : constant String := Ada.Directories.Base_Name
              (Ada.Command_Line.Command_Name);

            File : constant String := Ada.Directories.Compose
              (Path, Main, "xml");
         begin
            Write_JUnit_XML (File);
         end;
      end if;

      Ada.Text_IO.Put_Line
        (Ada.Strings.Unbounded.To_String (Controller.Testsuite_Set.Name)
         & ':');

      for Testsuite of Controller.Testsuite_Set.Testsuites loop
         Ada.Text_IO.Put_Line
           ("  " & Ada.Strings.Unbounded.To_String (Testsuite.Name) & ':');

         for Testcase of Testsuite.Testcases loop
            Ada.Text_IO.Put_Line
              ("    " & Ada.Strings.Unbounded.To_String (Testcase.Name)
               & ": " & Testcase_Status'Image (Testcase.Status)
               & (if Verbose and then Testcase.Message /= ""
                  then Ada.Characters.Latin_1.HT &
                   Ada.Strings.Unbounded.To_String (Testcase.Message)
                  else ""));

            if Verbose and Testcase.Traceback /= "" then
               Ada.Text_IO.Put_Line
                 (Ada.Strings.Unbounded.To_String (Testcase.Traceback));

               Ada.Text_IO.New_Line;
            end if;
         end loop;
      end loop;

      if (for some Testsuite of Controller.Testsuite_Set.Testsuites =>
           (for some Testcase of Testsuite.Testcases =>
             Testcase.Status /= Succeed))
      then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("SOME TESTCASE HAS NOT SUCCEED!");

         if not Verbose then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put ("Run with `--verbose` option or ");
            Ada.Text_IO.Put ("VERBOSE_TEST_REPORT environment set ");
            Ada.Text_IO.Put_Line ("to see more info.");
         end if;
      end if;
   exception
      when E : others =>
         --  Handle all exceptions in the finalization.
         --  GDB can't catch them, because they raised in the runtime
         --  finalization and this makes hard to debug.
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Finalize;

   --------------------------------
   -- Global_Unhandled_Exception --
   --------------------------------

   procedure Global_Unhandled_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      use type Ada.Exceptions.Exception_Id;

   begin
      if Controller.Active_Testcase.Name = "" then
         Start_Testcase (Default_Testcase);
      end if;

      --  Set status of the active testcase depending of the unhandled
      --  exception. It is case when default testsuite and default testcase
      --  are used.

      if Ada.Exceptions.Exception_Identity (Occurrence)
           = Test_Failed'Identity
      then
         Controller.Active_Testcase.Status := Failed;

      elsif Ada.Exceptions.Exception_Identity (Occurrence)
              = Test_Skipped'Identity
      then
         Controller.Active_Testcase.Status := Skipped;

      else
         Controller.Active_Testcase.Status := Errored;
      end if;
   end Global_Unhandled_Exception;

   ------------------
   -- Run_Testcase --
   ------------------

   procedure Run_Testcase
     (Testcase : not null access procedure;
      Name     : String;
      Message  : String := "";
      Location : String := GNAT.Source_Info.Source_Location)
   is
      pragma Unreferenced (Message, Location);

   begin
      Start_Testcase (Name);

      Testcase.all;

      End_Testcase;

   exception
      when Test_Failed =>
         Controller.Active_Testcase.Status := Failed;
         End_Testcase;

      when Test_Skipped =>
         Controller.Active_Testcase.Status := Skipped;
         End_Testcase;

      when E : others =>
         Controller.Active_Testcase.Status := Errored;

         Controller.Active_Testcase.Message :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Ada.Exceptions.Exception_Message (E));

         Controller.Active_Testcase.Traceback :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Ada.Exceptions.Exception_Information (E));

         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

         End_Testcase;
   end Run_Testcase;

   -------------------
   -- Run_Testsuite --
   -------------------

   procedure Run_Testsuite
     (Testsuite : not null access procedure;
      Name      : String;
      Message   : String := "";
      Location  : String := GNAT.Source_Info.Source_Location)
   is
      pragma Unreferenced (Message, Location);

   begin
      Start_Testsuite (Name);

      Testsuite.all;

      End_Testsuite;

   exception
      when others =>
         End_Testsuite;
   end Run_Testsuite;

   ----------
   -- Skip --
   ----------

   procedure Skip
     (Message  : String := "";
      Location : String := GNAT.Source_Info.Source_Location)
   is
      pragma Unreferenced (Location);

   begin
      if Controller.Active_Testcase.Name = "" then
         --  Start default testcase.

         Start_Testcase (Default_Testcase);
      end if;

      Controller.Active_Testcase.Message :=
        Ada.Strings.Unbounded.To_Unbounded_String (Message);

      raise Test_Skipped;
   end Skip;

   --------------------
   -- Start_Testcase --
   --------------------

   procedure Start_Testcase (Name : String) is
   begin
      if Controller.Active_Testcase.Name /= "" then
         raise Program_Error;
         --  XXX Nested testcases is not supported.
      end if;

      if Controller.Active_Testsuite.Name = "" then
         --  Start default testsuite.

         Start_Testsuite (Default_Testsuite);
      end if;

      Controller.Active_Testcase :=
        (Name       => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Status     => Succeed,
         Message    => <>,
         Traceback  => <>,
         Assertions => 0);
   end Start_Testcase;

   ---------------------
   -- Start_Testsuite --
   ---------------------

   procedure Start_Testsuite (Name : String) is
   begin
      if Controller.Active_Testsuite.Name /= "" then
         raise Program_Error;
         --  XXX Nested testsuites not implemented.
      end if;

      Controller.Active_Testsuite :=
        (Name      => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Testcases => <>);
   end Start_Testsuite;

   ---------------------
   -- Write_JUnit_XML --
   ---------------------

   procedure Write_JUnit_XML (File : String) is
      Output : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (Output, Name => File, Form => "WCEM=8");

      Ada.Text_IO.Put_Line
        (Output, "<?xml version='1.0' encoding='UTF-8' ?>");

      Ada.Text_IO.Put (Output, "<testsuites name='");

      Ada.Text_IO.Put
        (Output,
         Ada.Strings.Unbounded.To_String (Controller.Testsuite_Set.Name));

      Ada.Text_IO.Put_Line (Output, "'>");

      for Testsuite of Controller.Testsuite_Set.Testsuites loop

         Ada.Text_IO.Put (Output, " <testsuite name='");

         Ada.Text_IO.Put
           (Output, Ada.Strings.Unbounded.To_String (Testsuite.Name));

         Ada.Text_IO.Put_Line (Output, "'>");

         for Testcase of Testsuite.Testcases loop

            Ada.Text_IO.Put (Output, "  <testcase name='");

            Ada.Text_IO.Put
              (Output,
               Ada.Strings.Unbounded.To_String
                 (Escape_Attribute_Value (Testcase.Name)));

            Ada.Text_IO.Put (Output, "' assertions='");
            Ada.Text_IO.Put
              (Output,
               Ada.Strings.Fixed.Trim
                 (Natural'Image (Testcase.Assertions), Ada.Strings.Both));
            Ada.Text_IO.Put (Output, "'");

            case Testcase.Status is
               when Unknown =>
                  Ada.Text_IO.Put_Line
                    (Output,
                     ">BAD TESTSUITE: Unknown testcase status</testcase>");

               when Succeed =>
                  Ada.Text_IO.Put_Line (Output, "/>");

               when Failed =>
                  Ada.Text_IO.Put (Output, "><failure message='");

                  Ada.Text_IO.Put
                    (Output,
                     Ada.Strings.Unbounded.To_String (Testcase.Message));

                  Ada.Text_IO.Put_Line (Output, "'>");

                  Ada.Text_IO.Put_Line
                    (Output,
                     Ada.Strings.Unbounded.To_String (Testcase.Traceback));

                  Ada.Text_IO.Put_Line (Output, "</failure></testcase>");

               when Errored =>
                  Ada.Text_IO.Put_Line (Output, "><error>");

                  Ada.Text_IO.Put
                    (Output,
                     Ada.Strings.Unbounded.To_String (Testcase.Message));

                  Ada.Text_IO.Put_Line
                    (Output,
                     Ada.Strings.Unbounded.To_String (Testcase.Traceback));

                  Ada.Text_IO.Put_Line (Output, "</error></testcase>");

               when Skipped =>
                  --  There is no clear definition of use of the 'message'
                  --  attribute of the 'skipped' tag. It is generated for
                  --  compatibility with e3's XUnit XML convertor'.

                  Ada.Text_IO.Put
                    (Output, "><skipped message='");
                  Ada.Text_IO.Put
                    (Output,
                     Ada.Strings.Unbounded.To_String (Testcase.Message));
                  Ada.Text_IO.Put_Line
                    (Output, "'/></testcase>");
            end case;
         end loop;

         Ada.Text_IO.Put_Line (Output, " </testsuite>");

      end loop;

      Ada.Text_IO.Put_Line (Output, "</testsuites>");
      Ada.Text_IO.Close (Output);
   end Write_JUnit_XML;

begin
   Controller.Testsuite_Set.Name :=
     Ada.Strings.Unbounded.To_Unbounded_String (Ada.Command_Line.Command_Name);
   GNAT.Exception_Actions.Register_Global_Unhandled_Action
     (Global_Unhandled_Exception'Access);
end Test_Support;
