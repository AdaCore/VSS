--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.JSON.Streams.Cursors is

   -------------
   -- Element --
   -------------

   function Element
     (Self : JSON_Stream_Cursor'Class)
      return VSS.JSON.Streams.JSON_Stream_Element is
   begin
      case Self.Element_Kind is
         when None =>
            return (Kind => None);

         when Invalid =>
            return (Kind => Invalid);

         when Start_Document =>
            return (Kind => Start_Document);

         when End_Document =>
            return (Kind => End_Document);

         when Comment =>
            raise Program_Error;
            --  XXX Not implemented.

         when Start_Array =>
            return (Kind => Start_Array);

         when End_Array =>
            return (Kind => End_Array);

         when Start_Object =>
            return (Kind => Start_Object);

         when End_Object =>
            return (Kind => End_Object);

         when Key_Name =>
            return (Kind => Key_Name, Key_Name => Self.Key_Name);

         when String_Value =>
            return (Kind => String_Value, String_Value => Self.String_Value);

         when Number_Value =>
            return (Kind => Number_Value, Number_Value => Self.Number_Value);

         when Boolean_Value =>
            return
              (Kind => Boolean_Value, Boolean_Value => Self.Boolean_Value);

         when Null_Value =>
            return (Kind => Null_Value);
      end case;
   end Element;

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
