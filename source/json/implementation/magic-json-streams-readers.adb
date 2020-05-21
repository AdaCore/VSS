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

package body Magic.JSON.Streams.Readers is

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Error /= No_Error;
   end Has_Error;

   ----------------------
   -- Is_Boolean_Value --
   ----------------------

   function Is_Boolean_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = Boolean_Value;
   end Is_Boolean_Value;

   ------------------
   -- Is_End_Array --
   ------------------

   function Is_End_Array (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = End_Array;
   end Is_End_Array;

   ---------------------
   -- Is_End_Document --
   ---------------------

   function Is_End_Document (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = End_Document;
   end Is_End_Document;

   -------------------
   -- Is_End_Object --
   -------------------

   function Is_End_Object (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = End_Object;
   end Is_End_Object;

   -----------------
   -- Is_Key_Name --
   -----------------

   function Is_Key_Name (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = Key_Name;
   end Is_Key_Name;

   -------------------
   -- Is_Null_Value --
   -------------------

   function Is_Null_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = Null_Value;
   end Is_Null_Value;

   ---------------------
   -- Is_Number_Value --
   ---------------------

   function Is_Number_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = Number_Value;
   end Is_Number_Value;

   --------------------
   -- Is_Start_Array --
   --------------------

   function Is_Start_Array (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = Start_Array;
   end Is_Start_Array;

   -----------------------
   -- Is_Start_Document --
   -----------------------

   function Is_Start_Document
     (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = Start_Document;
   end Is_Start_Document;

   ---------------------
   -- Is_Start_Object --
   ---------------------

   function Is_Start_Object (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = Start_Object;
   end Is_Start_Object;

   ---------------------
   -- Is_String_Value --
   ---------------------

   function Is_String_Value (Self : JSON_Stream_Reader'Class) return Boolean is
   begin
      return Self.Event_Kind = String_Value;
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
