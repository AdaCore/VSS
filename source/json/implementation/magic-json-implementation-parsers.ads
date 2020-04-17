------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--  Internal implementation of JSON parser.
--
--  This parser supports normal parsing model as well as incremental parsing.
--  It use Input_Text_Stream interface as data source.

private with Ada.Strings.Wide_Wide_Unbounded;

with Magic.JSON.Streams.Readers;
with Magic.Strings;
with Magic.Text_Streams;
private with Magic.Unicode;

package Magic.JSON.Implementation.Parsers is

   type JSON_Parser is tagged limited private;

   procedure Set_Stream
     (Self   : in out JSON_Parser'Class;
      Stream : not null Magic.Text_Streams.Input_Text_Stream_Access);

   procedure Parse (Self : in out JSON_Parser'Class);
   --  Parse single token.

   function Event_Kind
     (Self : JSON_Parser'Class)
      return Magic.JSON.Streams.Readers.JSON_Event_Kind;
   --  Returns current event.

   function Error
     (Self : JSON_Parser'Class)
      return Magic.JSON.Streams.Readers.JSON_Reader_Error;
   --  Return current error.

   function String_Value
     (Self : JSON_Parser'Class) return Magic.Strings.Magic_String;
   --  Return string data (key name or string value)

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
      Stack : Parse_State_Array (1 .. 32);
   end record;

   function Is_Empty (Self : Parse_Stack'Class) return Boolean;

   function Top (Self : Parse_Stack'Class) return Parse_State;

   procedure Push
     (Self  : in out Parse_Stack'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32);

   procedure Pop (Self : in out Parse_Stack'Class);

   type JSON_Parser is tagged limited record
      Stream      : Magic.Text_Streams.Input_Text_Stream_Access;
      Stack       : Parse_Stack;
      Nesting     : Natural := 0;
      Event       : Magic.JSON.Streams.Readers.JSON_Event_Kind :=
        Magic.JSON.Streams.Readers.No_Token;
      Error       : Magic.JSON.Streams.Readers.JSON_Reader_Error :=
        Magic.JSON.Streams.Readers.No_Error;
      C           : Wide_Wide_Character;
      String      : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Code_Unit_1 : Magic.Unicode.UTF16_Code_Unit;
      Code_Unit_2 : Magic.Unicode.UTF16_Code_Unit;
   end record;

end Magic.JSON.Implementation.Parsers;
