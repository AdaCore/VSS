--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Common code for implementation of JSON parsers.

with VSS.JSON.Pull_Readers;
with VSS.JSON.Streams;
with VSS.Text_Streams;

package VSS.JSON.Implementation.Parsers is

   pragma Preelaborate;

   type JSON_Parser_Base is abstract tagged limited private;

   procedure Set_Stream
     (Self   : in out JSON_Parser_Base'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access);

   function At_End (Self : JSON_Parser_Base'Class) return Boolean;
   --  Return True when end of document has been processed.

   function Element_Kind
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind;
   --  Return current event.

   function Error
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error;
   --  Return current error.

   function Error_Message
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String;
   --  Return error message.

   function String_Value
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String;
   --  Return string data (key name or string value)

   function Boolean_Value (Self : JSON_Parser_Base'Class) return Boolean;
   --  Return boolean value

   function Number_Value
     (Self : JSON_Parser_Base'Class) return VSS.JSON.JSON_Number;
   --  Return number value

private

   type Parse_Subprogram is
     access function (Parser : in out JSON_Parser_Base'Class) return Boolean;

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

   type JSON_Parser_Base is abstract tagged limited record
      Stream  : VSS.Text_Streams.Input_Text_Stream_Access;

      Error   : VSS.JSON.Pull_Readers.JSON_Reader_Error :=
        VSS.JSON.Pull_Readers.No_Error;
      Message : VSS.Strings.Virtual_String;

      Event   : VSS.JSON.Streams.JSON_Stream_Element_Kind :=
        VSS.JSON.Streams.None;
      Buffer  : VSS.Strings.Virtual_String;
      Boolean : Standard.Boolean;
      Number  : VSS.JSON.JSON_Number;

      Stack   : Parse_Stack;
      C       : Wide_Wide_Character;
      --  Currently processed character. When end of stream is reached it sets
      --  to End_Of_Stream value.
   end record;

   function Push
     (Self  : in out JSON_Parser_Base'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) return Boolean
     with Post => Push'Result = False;
   --  Store state in the recovery stack. Do nothing if object is in error
   --  state.

   function Read
     (Self  : in out JSON_Parser_Base'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) return Boolean;
   --  Attempt to read next character from the text stream. Return True is
   --  operation is successful; otherwise push (Parse, State) pair into the
   --  parser's state stack and return False.

   function Report_Error
     (Self    : in out JSON_Parser_Base'Class;
      Message : Wide_Wide_String) return Boolean;
   --  Set parser into document not valid state. Always return False.

   End_Of_Stream : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#1F_FFFF#);

end VSS.JSON.Implementation.Parsers;
