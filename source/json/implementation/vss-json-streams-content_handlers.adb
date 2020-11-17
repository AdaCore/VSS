------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Assertions;

package body VSS.JSON.Streams.Content_Handlers is

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

end VSS.JSON.Streams.Content_Handlers;
