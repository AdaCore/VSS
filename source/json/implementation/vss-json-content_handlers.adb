--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Assertions;

package body VSS.JSON.Content_Handlers is

   -------------------
   -- Boolean_Value --
   -------------------

   procedure Boolean_Value
     (Self  : in out JSON_Content_Handler'Class;
      Value : Boolean)
   is
      Success : Boolean := True;

   begin
      Self.Boolean_Value (Value, Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Boolean_Value;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out JSON_Content_Handler'Class) is
      Success : Boolean := True;

   begin
      Self.End_Array (Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end End_Array;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Self : in out JSON_Content_Handler'Class) is
      Success : Boolean := True;

   begin
      Self.End_Document (Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end End_Document;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out JSON_Content_Handler'Class) is
      Success : Boolean := True;

   begin
      Self.End_Object (Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end End_Object;

   -----------------
   -- Float_Value --
   -----------------

   procedure Float_Value
     (Self    : in out JSON_Content_Handler'Class;
      Value   : Interfaces.IEEE_Float_64;
      Success : in out Boolean) is
   begin
      Self.Number_Value
        ((VSS.JSON.JSON_Float, VSS.Strings.Empty_Virtual_String, Value),
         Success);
   end Float_Value;

   -----------------
   -- Float_Value --
   -----------------

   procedure Float_Value
     (Self  : in out JSON_Content_Handler'Class;
      Value : Interfaces.IEEE_Float_64)
   is
      Success : Boolean := True;

   begin
      Self.Float_Value (Value, Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Float_Value;

   -------------------
   -- Integer_Value --
   -------------------

   procedure Integer_Value
     (Self    : in out JSON_Content_Handler'Class;
      Value   : Interfaces.Integer_64;
      Success : in out Boolean) is
   begin
      Self.Number_Value
        ((VSS.JSON.JSON_Integer, VSS.Strings.Empty_Virtual_String, Value),
         Success);
   end Integer_Value;

   -------------------
   -- Integer_Value --
   -------------------

   procedure Integer_Value
     (Self  : in out JSON_Content_Handler'Class;
      Value : Interfaces.Integer_64)
   is
      Success : Boolean := True;

   begin
      Self.Integer_Value (Value, Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Integer_Value;

   --------------
   -- Key_Name --
   --------------

   procedure Key_Name
     (Self : in out JSON_Content_Handler'Class;
      Name : VSS.Strings.Virtual_String'Class)
   is
      Success : Boolean := True;

   begin
      Self.Key_Name (Name, Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Key_Name;

   ----------------
   -- Null_Value --
   ----------------

   procedure Null_Value (Self : in out JSON_Content_Handler'Class) is
      Success : Boolean := True;

   begin
      Self.Null_Value (Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Null_Value;

   ------------------
   -- Number_Value --
   ------------------

   procedure Number_Value
     (Self  : in out JSON_Content_Handler'Class;
      Value : VSS.JSON.JSON_Number)
   is
      Success : Boolean := True;

   begin
      Self.Number_Value (Value, Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Number_Value;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out JSON_Content_Handler'Class) is
      Success : Boolean := True;

   begin
      Self.Start_Array (Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Start_Array;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Self : in out JSON_Content_Handler'Class) is
      Success : Boolean := True;

   begin
      Self.Start_Document (Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Start_Document;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out JSON_Content_Handler'Class) is
      Success : Boolean := True;

   begin
      Self.Start_Object (Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end Start_Object;

   ------------------
   -- String_Value --
   ------------------

   procedure String_Value
     (Self  : in out JSON_Content_Handler'Class;
      Value : VSS.Strings.Virtual_String'Class)
   is
      Success : Boolean := True;

   begin
      Self.String_Value (Value, Success);

      if not Success then
         raise Ada.Assertions.Assertion_Error;
      end if;
   end String_Value;

end VSS.JSON.Content_Handlers;
