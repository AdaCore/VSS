------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

package body VSS.JSON.Streams.Pull.Readers.Simple is

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Self : JSON_Simple_Pull_Reader) return Boolean is
   begin
      return Self.Parser.At_End;
   end At_End;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : JSON_Simple_Pull_Reader) return Boolean is
   begin
      return Self.Parser.Boolean_Value;
   end Boolean_Value;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out JSON_Simple_Pull_Reader) is
   begin
      raise Program_Error;
   end Clear;

   -----------
   -- Error --
   -----------

   overriding function Error
     (Self : JSON_Simple_Pull_Reader)
      return VSS.JSON.Streams.Pull.Readers.JSON_Reader_Error is
   begin
      return Self.Parser.Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      return Self.Parser.Error_Message;
   end Error_Message;

   ----------------
   -- Event_Kind --
   ----------------

   overriding function Event_Kind
     (Self : JSON_Simple_Pull_Reader)
      return VSS.JSON.Streams.Pull.Readers.JSON_Event_Kind is
   begin
      return Self.Parser.Event_Kind;
   end Event_Kind;

   --------------
   -- Key_Name --
   --------------

   overriding function Key_Name
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      return Self.Parser.String_Value;
   end Key_Name;

   ------------------
   -- Number_Value --
   ------------------

   overriding function Number_Value
     (Self : JSON_Simple_Pull_Reader) return VSS.JSON.JSON_Number is
   begin
      return Self.Parser.Number_Value;
   end Number_Value;

   -----------------
   -- Raise_Error --
   -----------------

   overriding procedure Raise_Error
     (Self    : in out JSON_Simple_Pull_Reader;
      Message : VSS.Strings.Virtual_String) is
   begin
      raise Program_Error;
   end Raise_Error;

   ---------------
   -- Read_Next --
   ---------------

   overriding function Read_Next
     (Self : in out JSON_Simple_Pull_Reader)
      return VSS.JSON.Streams.Pull.Readers.JSON_Event_Kind is
   begin
      Self.Parser.Parse;

      return Self.Parser.Event_Kind;
   end Read_Next;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Simple_Pull_Reader'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Parser.Set_Stream (Stream);
   end Set_Stream;

   ------------------------
   -- Skip_Current_Array --
   ------------------------

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Simple_Pull_Reader) is
   begin
      pragma Assert (Self.Is_Start_Array);

      Self.Read_Next;

      while not Self.Is_End_Array loop
         Self.Skip_Current_Value;
      end loop;

      Self.Read_Next;  --  Skip End_Array
   end Skip_Current_Array;

   -------------------------
   -- Skip_Current_Object --
   -------------------------

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Simple_Pull_Reader) is
   begin
      pragma Assert (Self.Is_Start_Object);

      Self.Read_Next;

      while not Self.Is_End_Object loop
         pragma Assert (Self.Is_Key_Name);

         Self.Read_Next;
         Self.Skip_Current_Value;
      end loop;

      Self.Read_Next;  --  Skip End_Object
   end Skip_Current_Object;

   ------------------------
   -- Skip_Current_Value --
   ------------------------

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Simple_Pull_Reader) is
   begin
      case Self.Event_Kind is
         when No_Token =>
            raise Program_Error;
         when Invalid =>
            raise Program_Error;
         when Start_Document =>
            raise Program_Error;
         when End_Document =>
            raise Program_Error;
         when Start_Array =>
            Self.Skip_Current_Array;
         when End_Array =>
            raise Program_Error;
         when Start_Object =>
            Self.Skip_Current_Object;
         when End_Object =>
            raise Program_Error;
         when Key_Name =>
            raise Program_Error;
         when String_Value =>
            Self.Read_Next;
         when Number_Value =>
            Self.Read_Next;
         when Boolean_Value =>
            Self.Read_Next;
         when Null_Value =>
            Self.Read_Next;
      end case;
   end Skip_Current_Value;

   ------------------
   -- String_Value --
   ------------------

   overriding function String_Value
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      return Self.Parser.String_Value;
   end String_Value;

end VSS.JSON.Streams.Pull.Readers.Simple;
