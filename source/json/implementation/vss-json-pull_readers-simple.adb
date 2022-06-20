--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.JSON.Pull_Readers.Simple is

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
      return VSS.JSON.Pull_Readers.JSON_Reader_Error is
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
      return VSS.JSON.Pull_Readers.JSON_Event_Kind is
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
      return VSS.JSON.Pull_Readers.JSON_Event_Kind is
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

end VSS.JSON.Pull_Readers.Simple;
