--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Internal implementation of JSON parser.
--
--  This parser supports normal parsing model as well as incremental parsing.
--  It use Input_Text_Stream interface as data source.

private with VSS.JSON.Implementation.Numbers;
with VSS.JSON.Pull_Readers;
with VSS.Strings;
with VSS.Text_Streams;
private with VSS.Unicode;

package VSS.JSON.Implementation.Parsers is

   type JSON_Parser is tagged limited private;

   procedure Set_Stream
     (Self   : in out JSON_Parser'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access);

   procedure Parse (Self : in out JSON_Parser'Class);
   --  Parse single token.

   function At_End (Self : JSON_Parser'Class) return Boolean;
   --  Return True when end of document has been processed.

   function Event_Kind
     (Self : JSON_Parser'Class)
      return VSS.JSON.Pull_Readers.JSON_Event_Kind;
   --  Return current event.

   function Error
     (Self : JSON_Parser'Class)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error;
   --  Return current error.

   function Error_Message
     (Self : JSON_Parser'Class) return VSS.Strings.Virtual_String;
   --  Return error message.

   function String_Value
     (Self : JSON_Parser'Class) return VSS.Strings.Virtual_String;
   --  Return string data (key name or string value)

   function Boolean_Value (Self : JSON_Parser'Class) return Boolean;
   --  Return boolean value

   function Number_Value
     (Self : JSON_Parser'Class) return VSS.JSON.JSON_Number;
   --  Return number value

private

   type Parse_Subprogram is
     access function (Self : in out JSON_Parser'Class) return Boolean;

   type Parse_State is record
      Parse : Parse_Subprogram;
      State : Interfaces.Unsigned_32;
   end record;

   type Parse_State_Array is array (Positive range <>) of Parse_State;

   type Parse_Stack is tagged limited record
      Head  : Natural := 0;
      Stack : Parse_State_Array (1 .. 64);
   end record;

   function Is_Empty (Self : Parse_Stack'Class) return Boolean;

   function Top (Self : Parse_Stack'Class) return Parse_State;

   procedure Push
     (Self  : in out Parse_Stack'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32);

   procedure Pop (Self : in out Parse_Stack'Class);

   type JSON_Parser is tagged limited record
      Stream       : VSS.Text_Streams.Input_Text_Stream_Access;
      Stack        : Parse_Stack;
      Event        : VSS.JSON.Pull_Readers.JSON_Event_Kind :=
        VSS.JSON.Pull_Readers.No_Token;
      Error        : VSS.JSON.Pull_Readers.JSON_Reader_Error :=
        VSS.JSON.Pull_Readers.No_Error;
      Message      : VSS.Strings.Virtual_String;
      C            : Wide_Wide_Character;
      Buffer       : VSS.Strings.Virtual_String;
      Boolean      : Standard.Boolean;
      Number       : VSS.JSON.JSON_Number;
      Code_Unit_1  : VSS.Unicode.UTF16_Code_Unit;
      Code_Unit_2  : VSS.Unicode.UTF16_Code_Unit;
      Number_State : VSS.JSON.Implementation.Numbers.Parsing_State;
   end record;

   procedure Push
     (Self  : in out JSON_Parser'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32);

end VSS.JSON.Implementation.Parsers;
