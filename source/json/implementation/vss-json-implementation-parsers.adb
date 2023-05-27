--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.JSON.Implementation.Parsers is

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value (Self : JSON_Parser_Base'Class) return Boolean is
   begin
      return Self.Boolean;
   end Boolean_Value;

   -----------
   -- Error --
   -----------

   function Error
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error is
   begin
      return Self.Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Message;
   end Error_Message;

   ----------------
   -- Event_Kind --
   ----------------

   function Event_Kind
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Pull_Readers.JSON_Event_Kind is
   begin
      return Self.Event;
   end Event_Kind;

   ------------------
   -- Number_Value --
   ------------------

   function Number_Value
     (Self : JSON_Parser_Base'Class) return VSS.JSON.JSON_Number is
   begin
      return Self.Number;
   end Number_Value;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Parser_Base'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Stream := Stream;
   end Set_Stream;

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Buffer;
   end String_Value;

end VSS.JSON.Implementation.Parsers;
