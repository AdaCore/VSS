--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.JSON.Streams.Cursors is

   ----------------------
   -- Is_Boolean_Value --
   ----------------------

   function Is_Boolean_Value
     (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = Boolean_Value;
   end Is_Boolean_Value;

   ------------------
   -- Is_End_Array --
   ------------------

   function Is_End_Array (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = End_Array;
   end Is_End_Array;

   ---------------------
   -- Is_End_Document --
   ---------------------

   function Is_End_Document (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = End_Document;
   end Is_End_Document;

   -------------------
   -- Is_End_Object --
   -------------------

   function Is_End_Object (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = End_Object;
   end Is_End_Object;

   -----------------
   -- Is_Key_Name --
   -----------------

   function Is_Key_Name (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = Key_Name;
   end Is_Key_Name;

   -------------------
   -- Is_Null_Value --
   -------------------

   function Is_Null_Value (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = Null_Value;
   end Is_Null_Value;

   ---------------------
   -- Is_Number_Value --
   ---------------------

   function Is_Number_Value (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = Number_Value;
   end Is_Number_Value;

   --------------------
   -- Is_Start_Array --
   --------------------

   function Is_Start_Array (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = Start_Array;
   end Is_Start_Array;

   -----------------------
   -- Is_Start_Document --
   -----------------------

   function Is_Start_Document
     (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = Start_Document;
   end Is_Start_Document;

   ---------------------
   -- Is_Start_Object --
   ---------------------

   function Is_Start_Object (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = Start_Object;
   end Is_Start_Object;

   ---------------------
   -- Is_String_Value --
   ---------------------

   function Is_String_Value (Self : JSON_Stream_Cursor'Class) return Boolean is
   begin
      return Self.Element_Kind = String_Value;
   end Is_String_Value;

end VSS.JSON.Streams.Cursors;
