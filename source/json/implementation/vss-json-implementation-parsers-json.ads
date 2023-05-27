--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Internal implementation of JSON parser.
--
--  This parser supports normal parsing model as well as incremental parsing.
--  It use Input_Text_Stream interface as data source.

private with VSS.JSON.Implementation.Numbers;
private with VSS.Unicode;

package VSS.JSON.Implementation.Parsers.JSON is

   type JSON_Parser is limited new JSON_Parser_Base with private;

   procedure Parse (Self : in out JSON_Parser'Class);
   --  Parse single token.

   function At_End (Self : JSON_Parser'Class) return Boolean;
   --  Return True when end of document has been processed.

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

   type JSON_Parser is limited new JSON_Parser_Base with record
      Stack        : Parse_Stack;
      C            : Wide_Wide_Character;
      Code_Unit_1  : VSS.Unicode.UTF16_Code_Unit;
      Code_Unit_2  : VSS.Unicode.UTF16_Code_Unit;
      Number_State : VSS.JSON.Implementation.Numbers.Parsing_State;
   end record;

   function Push
     (Self  : in out JSON_Parser'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) return Boolean
     with Post => Push'Result = False;
   --  Store state in the recovery stack. Do nothing if object is in error
   --  state.

end VSS.JSON.Implementation.Parsers.JSON;
