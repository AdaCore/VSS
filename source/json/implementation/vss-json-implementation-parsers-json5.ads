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

package VSS.JSON.Implementation.Parsers.JSON5 is

   type JSON5_Parser is limited new JSON_Parser_Base with private;

   procedure Parse (Self : in out JSON5_Parser'Class);
   --  Parse single token.

private

   type JSON5_Parser is limited new JSON_Parser_Base with record
      Unsigned     : Interfaces.Unsigned_64;
      Code_Unit_1  : VSS.Unicode.UTF16_Code_Unit;
      Code_Unit_2  : VSS.Unicode.UTF16_Code_Unit;
      Number_State : VSS.JSON.Implementation.Numbers.Parsing_State;
   end record;

end VSS.JSON.Implementation.Parsers.JSON5;
