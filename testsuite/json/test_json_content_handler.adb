--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Test raising of exception by JSON_Content_Handler's conventions
--  subprograms without Success parameter.

with Ada.Assertions;

with VSS.JSON.Content_Handlers;
with VSS.Strings;

with Test_Support;

procedure Test_JSON_Content_Handler is

   procedure Test_JSON_Content_Handler;

   type Test_Content_Handler is
     limited new VSS.JSON.Content_Handlers.JSON_Content_Handler
   with record
      Status : Boolean := False;
   end record;

   overriding procedure Start_Document
     (Self : in out Test_Content_Handler; Success : in out Boolean);

   overriding procedure End_Document
     (Self : in out Test_Content_Handler; Success : in out Boolean);

   overriding procedure Start_Array
     (Self : in out Test_Content_Handler; Success : in out Boolean);

   overriding procedure End_Array
     (Self : in out Test_Content_Handler; Success : in out Boolean);

   overriding procedure Start_Object
     (Self : in out Test_Content_Handler; Success : in out Boolean);

   overriding procedure End_Object
     (Self : in out Test_Content_Handler; Success : in out Boolean);

   overriding procedure Key_Name
     (Self    : in out Test_Content_Handler;
      Name    : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean);

   overriding procedure String_Value
     (Self    : in out Test_Content_Handler;
      Value   : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean);

   overriding procedure Number_Value
     (Self    : in out Test_Content_Handler;
      Value   : VSS.JSON.JSON_Number;
      Success : in out Boolean);

   overriding procedure Boolean_Value
     (Self    : in out Test_Content_Handler;
      Value   : Boolean;
      Success : in out Boolean);

   overriding procedure Null_Value
     (Self : in out Test_Content_Handler; Success : in out Boolean);

   overriding function Error_Message
     (Self : Test_Content_Handler) return VSS.Strings.Virtual_String;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding procedure Boolean_Value
     (Self    : in out Test_Content_Handler;
      Value   : Boolean;
      Success : in out Boolean) is
   begin
      Success := Self.Status;
   end Boolean_Value;

   ---------------
   -- End_Array --
   ---------------

   overriding procedure End_Array
     (Self : in out Test_Content_Handler; Success : in out Boolean) is
   begin
      Success := Self.Status;
   end End_Array;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document
     (Self : in out Test_Content_Handler; Success : in out Boolean) is
   begin
      Success := Self.Status;
   end End_Document;

   ----------------
   -- End_Object --
   ----------------

   overriding procedure End_Object
     (Self : in out Test_Content_Handler; Success : in out Boolean) is
   begin
      Success := Self.Status;
   end End_Object;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : Test_Content_Handler) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   --------------
   -- Key_Name --
   --------------

   overriding procedure Key_Name
     (Self    : in out Test_Content_Handler;
      Name    : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean) is
   begin
      Success := Self.Status;
   end Key_Name;

   ----------------
   -- Null_Value --
   ----------------

   overriding procedure Null_Value
     (Self : in out Test_Content_Handler; Success : in out Boolean) is
   begin
      Success := Self.Status;
   end Null_Value;

   ------------------
   -- Number_Value --
   ------------------

   overriding procedure Number_Value
     (Self    : in out Test_Content_Handler;
      Value   : VSS.JSON.JSON_Number;
      Success : in out Boolean) is
   begin
      Success := Self.Status;
   end Number_Value;

   -----------------
   -- Start_Array --
   -----------------

   overriding procedure Start_Array
     (Self : in out Test_Content_Handler; Success : in out Boolean) is
   begin
      Success := Self.Status;
   end Start_Array;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
     (Self : in out Test_Content_Handler; Success : in out Boolean) is
   begin
      Success := Self.Status;
   end Start_Document;

   ------------------
   -- Start_Object --
   ------------------

   overriding procedure Start_Object
     (Self : in out Test_Content_Handler; Success : in out Boolean) is
   begin
      Success := Self.Status;
   end Start_Object;

   ------------------
   -- String_Value --
   ------------------

   overriding procedure String_Value
     (Self    : in out Test_Content_Handler;
      Value   : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean) is
   begin
      Success := Self.Status;
   end String_Value;

   -------------------------------
   -- Test_JSON_Content_Handler --
   -------------------------------

   procedure Test_JSON_Content_Handler is
      Handler : aliased Test_Content_Handler;

      procedure Test_Start_Document;
      --  Start_Document

      procedure Test_End_Document;
      --  End_Document

      procedure Test_Start_Array;
      --  Start_Array

      procedure Test_End_Array;
      --  End_Array

      procedure Test_Start_Object;
      --  Start_Object

      procedure Test_End_Object;
      --  End_Object

      procedure Test_Key_Name;
      --  Key_Name

      procedure Test_String_Value;
      --  String_Value

      procedure Test_Number_Value;
      --  Number_Value

      procedure Test_Boolean_Value;
      --  Boolean_Value

      procedure Test_Null_Value;
      --  Null_Value

      procedure Test_Integer_Value;
      --  Integer_Value

      procedure Test_Float_Value;
      --  Float_Value

      ------------------------
      -- Test_Boolean_Value --
      ------------------------

      procedure Test_Boolean_Value is
      begin
         Handler.Status := True;
         Handler.Boolean_Value (False);

         begin
            Handler.Status := False;
            Handler.Boolean_Value (False);

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Boolean_Value;

      --------------------
      -- Test_End_Array --
      --------------------

      procedure Test_End_Array is
      begin
         Handler.Status := True;
         Handler.End_Array;

         begin
            Handler.Status := False;
            Handler.End_Array;

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_End_Array;

      -----------------------
      -- Test_End_Document --
      -----------------------

      procedure Test_End_Document is
      begin
         Handler.Status := True;
         Handler.End_Document;

         begin
            Handler.Status := False;
            Handler.End_Document;

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_End_Document;

      ---------------------
      -- Test_End_Object --
      ---------------------

      procedure Test_End_Object is
      begin
         Handler.Status := True;
         Handler.End_Object;

         begin
            Handler.Status := False;
            Handler.End_Object;

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_End_Object;

      ----------------------
      -- Test_Float_Value --
      ----------------------

      procedure Test_Float_Value is
      begin
         Handler.Status := True;
         Handler.Float_Value (0.0);

         begin
            Handler.Status := False;
            Handler.Float_Value (0.0);

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Float_Value;

      ------------------------
      -- Test_Integer_Value --
      ------------------------

      procedure Test_Integer_Value is
      begin
         Handler.Status := True;
         Handler.Integer_Value (0);

         begin
            Handler.Status := False;
            Handler.Integer_Value (0);

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Integer_Value;

      -------------------
      -- Test_Key_Name --
      -------------------

      procedure Test_Key_Name is
      begin
         Handler.Status := True;
         Handler.Key_Name (VSS.Strings.Empty_Virtual_String);

         begin
            Handler.Status := False;
            Handler.Key_Name (VSS.Strings.Empty_Virtual_String);

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Key_Name;

      ---------------------
      -- Test_Null_Value --
      ---------------------

      procedure Test_Null_Value is
      begin
         Handler.Status := True;
         Handler.Null_Value;

         begin
            Handler.Status := False;
            Handler.Null_Value;

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Null_Value;

      -----------------------
      -- Test_Number_Value --
      -----------------------

      procedure Test_Number_Value is
      begin
         Handler.Status := True;
         Handler.Number_Value ((others => <>));

         begin
            Handler.Status := False;
            Handler.Number_Value ((others => <>));

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Number_Value;

      ----------------------
      -- Test_Start_Array --
      ----------------------

      procedure Test_Start_Array is
      begin
         Handler.Status := True;
         Handler.Start_Array;

         begin
            Handler.Status := False;
            Handler.Start_Array;

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Start_Array;

      -------------------------
      -- Test_Start_Document --
      -------------------------

      procedure Test_Start_Document is
      begin
         Handler.Status := True;
         Handler.Start_Document;

         begin
            Handler.Status := False;
            Handler.Start_Document;

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Start_Document;

      -----------------------
      -- Test_Start_Object --
      -----------------------

      procedure Test_Start_Object is
      begin
         Handler.Status := True;
         Handler.Start_Object;

         begin
            Handler.Status := False;
            Handler.Start_Object;

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_Start_Object;

      -----------------------
      -- Test_String_Value --
      -----------------------

      procedure Test_String_Value is
      begin
         Handler.Status := True;
         Handler.String_Value (VSS.Strings.Empty_Virtual_String);

         begin
            Handler.Status := False;
            Handler.String_Value (VSS.Strings.Empty_Virtual_String);

            Test_Support.Fail;

         exception
            when Ada.Assertions.Assertion_Error =>
               null;
         end;
      end Test_String_Value;

   begin
      Test_Support.Run_Testcase (Test_Start_Document'Access, "Start_Document");
      Test_Support.Run_Testcase (Test_End_Document'Access, "End_Document");
      Test_Support.Run_Testcase (Test_Start_Array'Access, "Start_Array");
      Test_Support.Run_Testcase (Test_End_Array'Access, "End_Array");
      Test_Support.Run_Testcase (Test_Start_Object'Access, "Start_Object");
      Test_Support.Run_Testcase (Test_End_Object'Access, "End_Object");
      Test_Support.Run_Testcase (Test_Key_Name'Access, "Key_Name");
      Test_Support.Run_Testcase (Test_String_Value'Access, "String_Value");
      Test_Support.Run_Testcase (Test_Number_Value'Access, "Number_Value");
      Test_Support.Run_Testcase (Test_Boolean_Value'Access, "Boolean_Value");
      Test_Support.Run_Testcase (Test_Null_Value'Access, "Null_Value");
      Test_Support.Run_Testcase (Test_Integer_Value'Access, "Integer_Value");
      Test_Support.Run_Testcase (Test_Float_Value'Access, "Float_Value");
   end Test_JSON_Content_Handler;

begin
   Test_Support.Run_Testsuite
     (Test_JSON_Content_Handler'Access, "JSON_Content_Handler");
end Test_JSON_Content_Handler;
