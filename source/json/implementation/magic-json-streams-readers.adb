------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body Magic.JSON.Streams.Readers is

   ----------------------
   -- Is_Boolean_Value --
   ----------------------

   function Is_Boolean_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = Boolean_Value;
   end Is_Boolean_Value;

   ------------------
   -- Is_End_Array --
   ------------------

   function Is_End_Array (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = End_Array;
   end Is_End_Array;

   ---------------------
   -- Is_End_Document --
   ---------------------

   function Is_End_Document (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = End_Document;
   end Is_End_Document;

   -------------------
   -- Is_End_Object --
   -------------------

   function Is_End_Object (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = End_Object;
   end Is_End_Object;

   -----------------
   -- Is_Key_Name --
   -----------------

   function Is_Key_Name (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = Key_Name;
   end Is_Key_Name;

   -------------------
   -- Is_Null_Value --
   -------------------

   function Is_Null_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = Null_Value;
   end Is_Null_Value;

   ---------------------
   -- Is_Number_Value --
   ---------------------

   function Is_Number_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = Number_Value;
   end Is_Number_Value;

   --------------------
   -- Is_Start_Array --
   --------------------

   function Is_Start_Array (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = Start_Array;
   end Is_Start_Array;

   -----------------------
   -- Is_Start_Document --
   -----------------------

   function Is_Start_Document
     (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = Start_Document;
   end Is_Start_Document;

   ---------------------
   -- Is_Start_Object --
   ---------------------

   function Is_Start_Object (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = Start_Object;
   end Is_Start_Object;

   ---------------------
   -- Is_String_Value --
   ---------------------

   function Is_String_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Type = String_Value;
   end Is_String_Value;

   ---------------
   -- Read_Next --
   ---------------

   procedure Read_Next (Self : in out JSON_Stream_Reader'Class) is
      Dummy : constant JSON_Event_Kind := Self.Read_Next;

   begin
      null;
   end Read_Next;

end Magic.JSON.Streams.Readers;
