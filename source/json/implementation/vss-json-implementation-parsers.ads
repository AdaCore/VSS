--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Common code for implementation of JSON parsers.

private with VSS.Implementation.Character_Codes;
with VSS.JSON.Pull_Readers;
with VSS.JSON.Streams;
with VSS.Text_Streams;
private with VSS.Unicode;

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
      C       : VSS.Unicode.Code_Point_Unit;
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

   procedure Store_Character (Self : in out JSON_Parser_Base'Class);
   --  Append current character to the text buffer

   procedure Store_Character
     (Self : in out JSON_Parser_Base'Class;
      Code : VSS.Unicode.UTF16_Code_Unit);
   --  Append character with given code to the text buffer.

   procedure Store_Character
     (Self : in out JSON_Parser_Base'Class;
      High : VSS.Unicode.UTF16_Code_Unit;
      Low  : VSS.Unicode.UTF16_Code_Unit);
   --  Construct character from the given surrogate and append it to the text
   --  buffer.

   function Report_Error
     (Self    : in out JSON_Parser_Base'Class;
      Message : Wide_Wide_String) return Boolean;
   --  Set parser into document not valid state. Always return False.

   End_Of_Stream : constant VSS.Unicode.Code_Point_Unit :=
     VSS.Unicode.Code_Point_Unit'Last;

   Begin_Array     : constant :=
     VSS.Implementation.Character_Codes.Left_Square_Bracket;
   Begin_Object    : constant :=
     VSS.Implementation.Character_Codes.Left_Curly_Bracket;
   End_Array       : constant :=
     VSS.Implementation.Character_Codes.Right_Square_Bracket;
   End_Object      : constant :=
     VSS.Implementation.Character_Codes.Right_Curly_Bracket;
   Name_Separator  : constant :=
     VSS.Implementation.Character_Codes.Colon;
   Value_Separator : constant :=
     VSS.Implementation.Character_Codes.Comma;
   Decimal_Point   : constant :=
     VSS.Implementation.Character_Codes.Full_Stop;
   --  Common delimiter character codes.

end VSS.JSON.Implementation.Parsers;
