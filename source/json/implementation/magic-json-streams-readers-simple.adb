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

package body Magic.JSON.Streams.Readers.Simple is

   ------------
   -- At_End --
   ------------

   overriding function At_End (Self : JSON_Simple_Reader) return Boolean is
   begin
      raise Program_Error;
      return True;
   end At_End;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : JSON_Simple_Reader) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Boolean_Value;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out JSON_Simple_Reader) is
   begin
      raise Program_Error;
   end Clear;

   -----------
   -- Error --
   -----------

   overriding function Error
     (Self : JSON_Simple_Reader)
      return Magic.JSON.Streams.Readers.JSON_Reader_Error is
   begin
      return Self.Parser.Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : JSON_Simple_Reader) return Magic.Strings.Magic_String is
   begin
      raise Program_Error;
      return Magic.Strings.Empty_Magic_String;
   end Error_Message;

   ----------------
   -- Event_Kind --
   ----------------

   overriding function Event_Kind
     (Self : JSON_Simple_Reader)
      return Magic.JSON.Streams.Readers.JSON_Event_Kind is
   begin
      return Self.Parser.Event_Kind;
   end Event_Kind;

   --------------
   -- Key_Name --
   --------------

   overriding function Key_Name
     (Self : JSON_Simple_Reader) return Magic.Strings.Magic_String is
   begin
      raise Program_Error;
      return Magic.Strings.Empty_Magic_String;
   end Key_Name;

   ------------------
   -- Number_Value --
   ------------------

   overriding function Number_Value
     (Self : JSON_Simple_Reader) return Magic.JSON.JSON_Number is
   begin
      raise Program_Error;
      return (Kind => None);
   end Number_Value;

   -----------------
   -- Raise_Error --
   -----------------

   overriding procedure Raise_Error
     (Self    : in out JSON_Simple_Reader;
      Message : Magic.Strings.Magic_String :=
        Magic.Strings.Empty_Magic_String) is
   begin
      raise Program_Error;
   end Raise_Error;

   ---------------
   -- Read_Next --
   ---------------

   overriding function Read_Next
     (Self : in out JSON_Simple_Reader)
      return Magic.JSON.Streams.Readers.JSON_Event_Kind is
   begin
      Self.Parser.Parse;

      return Self.Parser.Event_Kind;
   end Read_Next;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Simple_Reader'Class;
      Stream : not null Magic.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Parser.Set_Stream (Stream);
   end Set_Stream;

   ------------------------
   -- Skip_Current_Array --
   ------------------------

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Simple_Reader) is
   begin
      raise Program_Error;
   end Skip_Current_Array;

   -------------------------
   -- Skip_Current_Object --
   -------------------------

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Simple_Reader) is
   begin
      raise Program_Error;
   end Skip_Current_Object;

   ------------------------
   -- Skip_Current_Value --
   ------------------------

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Simple_Reader) is
   begin
      raise Program_Error;
   end Skip_Current_Value;

   ------------------
   -- String_Value --
   ------------------

   overriding function String_Value
     (Self : JSON_Simple_Reader) return Magic.Strings.Magic_String is
   begin
      raise Program_Error;
      return Magic.Strings.Empty_Magic_String;
   end String_Value;

end Magic.JSON.Streams.Readers.Simple;
