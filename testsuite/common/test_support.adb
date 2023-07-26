--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Test_Support is

   use type Ada.Strings.Unbounded.Unbounded_String;

   type Testcase_Status is (Unknown, Succeed, Failed, Errored, Skipped);

   type Testcase_Information is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Status : Testcase_Status := Unknown;
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

   Controller : Test_Information;

   procedure Start_Testsuite (Name : String);

   procedure End_Testsuite;

   procedure Start_Testcase (Name : String);

   procedure End_Testcase;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Condition : Boolean;
      Message   : String := "";
      Location  : String := GNAT.Source_Info.Source_Location) is
   begin
      if not Condition then
         raise Test_Failed with "at "
                 & Location
                 & (if Message /= "" then " " & Message else "");
      end if;
   end Assert;

   ------------------
   -- End_Testcase --
   ------------------

   procedure End_Testcase is
   begin
      Controller.Active_Testsuite.Testcases.Append
        (Controller.Active_Testcase);
      Controller.Active_Testcase := (Name => <>, Status => Succeed);
   end End_Testcase;

   -------------------
   -- End_Testsuite --
   -------------------

   procedure End_Testsuite is
   begin
      Controller.Testsuite_Set.Testsuites.Append (Controller.Active_Testsuite);
      Controller.Active_Testsuite := (Name => <>, Testcases => <>);
   end End_Testsuite;

   ----------
   -- Fail --
   ----------

   procedure Fail
     (Message  : String := "";
      Location : String := GNAT.Source_Info.Source_Location)
   is
      pragma Unreferenced (Message, Location);

   begin
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      raise Test_Failed;
   end Fail;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Test_Information) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Strings.Unbounded.To_String (Controller.Testsuite_Set.Name)
         & ':');

      for Testsuite of Controller.Testsuite_Set.Testsuites loop
         Ada.Text_IO.Put_Line
           ("  " & Ada.Strings.Unbounded.To_String (Testsuite.Name) & ':');

         for Testcase of Testsuite.Testcases loop
            Ada.Text_IO.Put_Line
              ("    " & Ada.Strings.Unbounded.To_String (Testcase.Name)
               & ": " & Testcase_Status'Image (Testcase.Status));
         end loop;
      end loop;
   end Finalize;

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

      when others =>
         Controller.Active_Testcase.Status := Errored;
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
      pragma Unreferenced (Message, Location);

   begin
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

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

      Controller.Active_Testcase :=
        (Name   => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Status => Succeed);
   end Start_Testcase;

   ---------------------
   -- Start_Testsuite --
   ---------------------

   procedure Start_Testsuite (Name : String) is
   begin
      if Controller.Active_Testsuite.Name /= "" then
         raise Program_Error;
         --  XXX Not implemented.
      end if;

      Controller.Active_Testsuite :=
        (Name      => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Testcases => <>);
   end Start_Testsuite;

begin
   Controller.Testsuite_Set.Name :=
     Ada.Strings.Unbounded.To_Unbounded_String (Ada.Command_Line.Command_Name);
end Test_Support;
