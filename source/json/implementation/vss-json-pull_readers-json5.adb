--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.JSON.Pull_Readers.JSON5 is

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Self : JSON5_Pull_Reader) return Boolean is
   begin
      return Self.Parser.At_End;
   end At_End;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : JSON5_Pull_Reader) return Boolean is
   begin
      return Self.Parser.Boolean_Value;
   end Boolean_Value;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out JSON5_Pull_Reader) is
   begin
      raise Program_Error;
   end Clear;

   ------------------
   -- Element_Kind --
   ------------------

   overriding function Element_Kind
     (Self : JSON5_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      return Self.Parser.Element_Kind;
   end Element_Kind;

   -----------
   -- Error --
   -----------

   overriding function Error
     (Self : JSON5_Pull_Reader)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error is
   begin
      return Self.Parser.Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : JSON5_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      return Self.Parser.Error_Message;
   end Error_Message;

   --------------
   -- Key_Name --
   --------------

   overriding function Key_Name
     (Self : JSON5_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      return Self.Parser.String_Value;
   end Key_Name;

   ------------------
   -- Number_Value --
   ------------------

   overriding function Number_Value
     (Self : JSON5_Pull_Reader) return VSS.JSON.JSON_Number is
   begin
      return Self.Parser.Number_Value;
   end Number_Value;

   -----------------
   -- Raise_Error --
   -----------------

   overriding procedure Raise_Error
     (Self    : in out JSON5_Pull_Reader;
      Message : VSS.Strings.Virtual_String) is
   begin
      raise Program_Error;
   end Raise_Error;

   ---------------
   -- Read_Next --
   ---------------

   overriding function Read_Next
     (Self : in out JSON5_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      Self.Parser.Parse;

      return Self.Parser.Element_Kind;
   end Read_Next;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON5_Pull_Reader'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Parser.Set_Stream (Stream);
   end Set_Stream;

   ------------------------
   -- Skip_Current_Array --
   ------------------------

   overriding procedure Skip_Current_Array
     (Self : in out JSON5_Pull_Reader) is
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
     (Self : in out JSON5_Pull_Reader) is
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
     (Self : in out JSON5_Pull_Reader) is
   begin
      case Self.Element_Kind is
         when VSS.JSON.Streams.None =>
            raise Program_Error;
         when VSS.JSON.Streams.Invalid =>
            raise Program_Error;
         when VSS.JSON.Streams.Start_Document =>
            raise Program_Error;
         when VSS.JSON.Streams.End_Document =>
            raise Program_Error;
         when VSS.JSON.Streams.Comment =>
            Self.Read_Next;
         when VSS.JSON.Streams.Start_Array =>
            Self.Skip_Current_Array;
         when VSS.JSON.Streams.End_Array =>
            raise Program_Error;
         when VSS.JSON.Streams.Start_Object =>
            Self.Skip_Current_Object;
         when VSS.JSON.Streams.End_Object =>
            raise Program_Error;
         when VSS.JSON.Streams.Key_Name =>
            raise Program_Error;
         when VSS.JSON.Streams.String_Value =>
            Self.Read_Next;
         when VSS.JSON.Streams.Number_Value =>
            Self.Read_Next;
         when VSS.JSON.Streams.Boolean_Value =>
            Self.Read_Next;
         when VSS.JSON.Streams.Null_Value =>
            Self.Read_Next;
      end case;
   end Skip_Current_Value;

   ------------------
   -- String_Value --
   ------------------

   overriding function String_Value
     (Self : JSON5_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      return Self.Parser.String_Value;
   end String_Value;

end VSS.JSON.Pull_Readers.JSON5;
