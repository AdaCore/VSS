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

   type JSON_Parser is limited new JSON_Parser_Base with record
      Code_Unit_1  : VSS.Unicode.UTF16_Code_Unit;
      Code_Unit_2  : VSS.Unicode.UTF16_Code_Unit;
      Number_State : VSS.JSON.Implementation.Numbers.Parsing_State;
   end record;

end VSS.JSON.Implementation.Parsers.JSON;
