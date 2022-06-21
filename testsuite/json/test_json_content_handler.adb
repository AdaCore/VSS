--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Test raising of exception by JSON_Content_Handler's conventions
--  subprograms without Success parameter.

with Ada.Assertions;

with VSS.JSON.Content_Handlers;
with VSS.Strings;

procedure Test_JSON_Content_Handler is

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

   Handler : aliased Test_Content_Handler;

begin
   --  Start_Document

   Handler.Status := True;
   Handler.Start_Document;

   begin
      Handler.Status := False;
      Handler.Start_Document;

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  End_Document

   Handler.Status := True;
   Handler.End_Document;

   begin
      Handler.Status := False;
      Handler.End_Document;

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Start_Array

   Handler.Status := True;
   Handler.Start_Array;

   begin
      Handler.Status := False;
      Handler.Start_Array;

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  End_Array

   Handler.Status := True;
   Handler.End_Array;

   begin
      Handler.Status := False;
      Handler.End_Array;

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Start_Object

   Handler.Status := True;
   Handler.Start_Object;

   begin
      Handler.Status := False;
      Handler.Start_Object;

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  End_Object

   Handler.Status := True;
   Handler.End_Object;

   begin
      Handler.Status := False;
      Handler.End_Object;

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Key_Name

   Handler.Status := True;
   Handler.Key_Name (VSS.Strings.Empty_Virtual_String);

   begin
      Handler.Status := False;
      Handler.Key_Name (VSS.Strings.Empty_Virtual_String);

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  String_Value

   Handler.Status := True;
   Handler.String_Value (VSS.Strings.Empty_Virtual_String);

   begin
      Handler.Status := False;
      Handler.String_Value (VSS.Strings.Empty_Virtual_String);

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Number_Value

   Handler.Status := True;
   Handler.Number_Value ((others => <>));

   begin
      Handler.Status := False;
      Handler.Number_Value ((others => <>));

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Boolean_Value

   Handler.Status := True;
   Handler.Boolean_Value (False);

   begin
      Handler.Status := False;
      Handler.Boolean_Value (False);

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Null_Value

   Handler.Status := True;
   Handler.Null_Value;

   begin
      Handler.Status := False;
      Handler.Null_Value;

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Integer_Value

   Handler.Status := True;
   Handler.Integer_Value (0);

   begin
      Handler.Status := False;
      Handler.Integer_Value (0);

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;

   --  Float_Value

   Handler.Status := True;
   Handler.Float_Value (0.0);

   begin
      Handler.Status := False;
      Handler.Float_Value (0.0);

      raise Program_Error;

   exception
      when Ada.Assertions.Assertion_Error =>
         null;
   end;
end Test_JSON_Content_Handler;
