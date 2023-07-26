--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Package to support test control and report generation.
--
--  Typical use of the package is present in code snippet below.
--
--     procedure Test_Driver is
--
--        procedure Testcase is
--        begin
--           Test_Support.Assert (True /= False, "True is not False");
--           Test_Support.Assert (True = True, "True is True");
--           Test_Support.Assert (False = False, "False is False");
--        end Testcase;
--
--        procedure Testsuite is
--        begin
--           Test_Support.Run_Testcase (Testcase'Access, "equal-operator");
--           --  more calls of Run_Testcase
--        end Testsuite;
--
--     begin
--        Test_Support.Run_Testsuite (Testsuite'Access, "test of Boolean");
--     end Test_Driver;
--
--  Call of Assert with False contition terminates execution of the testcase.
--  Testcase execution can be terminated by the call of Fail subprogram, it
--  means that testcase fails, or by the call of Skip subprogram, it means
--  that testcase is not executed.
--
--  Testcase subprogram not need to catch exceptions, in case of unhandled
--  exception testcase's status is set to error.
--
--  If some testcase failed/errored/skipped, execution of other testcases and
--  testsuites continues.

with GNAT.Source_Info;

package Test_Support is

   pragma Elaborate_Body;

   procedure Run_Testsuite
     (Testsuite : not null access procedure;
      Name      : String;
      Message   : String := "";
      Location  : String := GNAT.Source_Info.Source_Location);
   --  Run given subprogram as testsuite.

   procedure Run_Testcase
     (Testcase : not null access procedure;
      Name     : String;
      Message  : String := "";
      Location : String := GNAT.Source_Info.Source_Location);
   --  Run given subprogram as testcase.

   procedure Assert
     (Condition : Boolean;
      Message   : String := "";
      Location  : String := GNAT.Source_Info.Source_Location);
   --  Check condition and terminates testcase execution when it is False.

   procedure Fail
     (Message  : String := "";
      Location : String := GNAT.Source_Info.Source_Location);
   --  Terminates testcase execution and mark testcase as failed.

   procedure Skip
     (Message  : String := "";
      Location : String := GNAT.Source_Info.Source_Location);
   --  Terminates testcase execution and mark testcase as skipped.

private

   Test_Failed  : exception;
   Test_Skipped : exception;

end Test_Support;
