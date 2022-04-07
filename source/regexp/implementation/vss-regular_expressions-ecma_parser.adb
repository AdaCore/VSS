------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body VSS.Regular_Expressions.ECMA_Parser is

   -------------------
   -- Parse_Pattern --
   -------------------

   procedure Parse_Pattern
     (Cursor : in out VSS.Strings.Character_Iterators.Character_Iterator;
      Error  : out VSS.Strings.Virtual_String;
      Result : out Node)
   is
      use type VSS.Characters.Virtual_Character;

      procedure Expect
        (Value : VSS.Characters.Virtual_Character;
         Ok    : in out Boolean);
      --  if Cursor points to Value, then forwart Cursor and set Ok to True,
      --  otherwise set Ok to False.

      procedure Alternative (Value : out Node; Ok : in out Boolean);
      procedure Disjunction (Value : out Node; Ok : in out Boolean);

      procedure Term (Value : out Node; Ok : in out Boolean)
        with Pre => Ok;

      procedure Atom (Value : out Node; Ok : in out Boolean)
        with Pre => Ok;

      procedure Atom_Escape (Value : out Node; Ok : in out Boolean)
        with Pre => Ok;

      procedure Unicode_Property_Value_Characters
        (Value : out VSS.Strings.Virtual_String; Ok : in out Boolean);

      procedure Lone_Unicode_Property_Name_Or_Value
        (Value : out Node; Ok : in out Boolean);

      procedure Character_Class (Value : out Node; Ok : in out Boolean)
        with Pre => Ok;

      procedure Class_Ranges (Value : out Node; Ok : in out Boolean);

      procedure Class_Atom
        (Value : out VSS.Characters.Virtual_Character;
         Ok    : in out Boolean);

      Next_Group : Positive := 1;  --  Group counter

      procedure Alternative (Value : out Node; Ok : in out Boolean) is
         Right : Node;
      begin
         if Ok and (not Cursor.Has_Element
                    or else Cursor.Element in ')' | '|')
         then
            Value := Create_Empty;

         elsif Ok then
            Term (Value, Ok);

            while Ok
              and then Cursor.Has_Element
              and then Cursor.Element not in ')' | '|'
            loop
               Term (Right, Ok);

               if Ok then
                  Value := Create_Sequence (Value, Right);
               end if;
            end loop;
         end if;
      end Alternative;

      procedure Atom (Value : out Node; Ok : in out Boolean) is
      begin
         if not Cursor.Has_Element then
            if Error.Is_Empty then
               Error := "Unexpected end of string while atom expected.";
            end if;

            Ok := False;
            return;
         end if;

         case Cursor.Element is
            when '(' =>
               Expect ('(', Ok);

               if Cursor.Element = '?' then
                  Expect ('?', Ok);
                  Expect (':', Ok);
                  Disjunction (Value, Ok);
                  Expect (')', Ok);
               else  --  empty GroupSpecifier
                  declare
                     Group : constant Positive := Next_Group;
                  begin
                     Next_Group := Next_Group + 1;
                     Disjunction (Value, Ok);
                     Value := Create_Group (Value, Group);
                     Expect (')', Ok);
                  end;
               end if;
            when '[' =>
               Character_Class (Value, Ok);

            when ')' | '|' =>
               raise Program_Error;

            when '\' =>
               Expect ('\', Ok);
               Atom_Escape (Value, Ok);

            when '^' | '$' | '.' | '*' | '+' | '?' | ']' | '{' | '}' =>
               if Error.Is_Empty then
                  Error := "Unexpected '";
                  Error.Append (Cursor.Element);
                  Error.Append ("' while atom expected.");
               end if;

               Ok := False;
            when others =>
               Value := Create_Character (Cursor.Element);
               Expect (Cursor.Element, Ok);
         end case;
      end Atom;

      procedure Atom_Escape (Value : out Node; Ok : in out Boolean) is
      begin
         Expect ('p', Ok);
         Expect ('{', Ok);
         Lone_Unicode_Property_Name_Or_Value (Value, Ok);
         Expect ('}', Ok);
      end Atom_Escape;

      procedure Character_Class (Value : out Node; Ok : in out Boolean) is
      begin
         Expect ('[', Ok);
         Class_Ranges (Value, Ok);
         Expect (']', Ok);
      end Character_Class;

      procedure Class_Atom
        (Value : out VSS.Characters.Virtual_Character;
         Ok    : in out Boolean) is
      begin
         if not Ok
           or else not Cursor.Has_Element
           or else Cursor.Element in '\' | ']'
         then
            Ok := False;
            Value := ' ';
            return;
         end if;

         Value := Cursor.Element;
         Expect (Cursor.Element, Ok);
      end Class_Atom;

      procedure Class_Ranges (Value : out Node; Ok : in out Boolean) is
         From : VSS.Characters.Virtual_Character;
         To   : VSS.Characters.Virtual_Character;
      begin
         Class_Atom (From, Ok);
         Expect ('-', Ok);
         Class_Atom (To, Ok);

         if Ok then
            Value := Create_Character_Range (From, To);
         end if;
      end Class_Ranges;

      procedure Disjunction (Value : out Node; Ok : in out Boolean) is
         Right : Node;
      begin
         Alternative (Value, Ok);

         while Ok and then
           Cursor.Has_Element and then
           Cursor.Element /= ')'
         loop
            Expect ('|', Ok);
            Alternative (Right, Ok);

            if Ok then
               Value := Create_Alternative (Value, Right);
            end if;
         end loop;
      end Disjunction;

      procedure Expect
        (Value : VSS.Characters.Virtual_Character;
         Ok    : in out Boolean) is
      begin
         if Ok then
            Ok := Cursor.Has_Element and then Cursor.Element = Value;

            if not Ok and not Error.Is_Empty then
               --  Error is already set. No other diagnostic is required.
               null;
            elsif not Cursor.Has_Element then
               Error := "Unexpected end of string while '";
               Error.Append (Value);
               Error.Append ("' expected.");
            elsif Cursor.Element /= Value then
               Error := "Got '";
               Error.Append (Cursor.Element);
               Error.Append ("' while '");
               Error.Append (Value);
               Error.Append ("' expected.");
            end if;

            Ok := Ok and then (Cursor.Forward or True);
         end if;
      end Expect;

      procedure Lone_Unicode_Property_Name_Or_Value
        (Value : out Node; Ok : in out Boolean)
      is
         Name : VSS.Strings.Virtual_String;
         Set  : Name_Sets.General_Category_Set;
      begin
         Unicode_Property_Value_Characters (Name, Ok);

         if Ok then
            Name_Sets.To_General_Category_Set (Name, Set, Ok);

            if Ok then
               Value := Create_General_Category_Set (Set);
            end if;
         end if;
      end Lone_Unicode_Property_Name_Or_Value;

      procedure Term (Value : out Node; Ok : in out Boolean) is
      begin
         Atom (Value, Ok);

         if Ok and then Cursor.Has_Element and then Cursor.Element = '*' then
            Expect (Cursor.Element, Ok);
            Value := Create_Star (Value);
         end if;
      end Term;

      procedure Unicode_Property_Value_Characters
        (Value : out VSS.Strings.Virtual_String; Ok : in out Boolean) is
      begin
         if Ok then
            Value.Clear;

            Ok := False;

            while Cursor.Has_Element and then
              Cursor.Element in 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9'
            loop
               Value.Append (Cursor.Element);
               Ok := Cursor.Forward or True;
            end loop;
         end if;
      end Unicode_Property_Value_Characters;

      Ok : Boolean := True;
   begin
      Disjunction (Result, Ok);

      if not Error.Is_Empty then
         null;
      elsif not Ok or Cursor.Has_Element then
         Error := "Unable to parse";
      end if;
   end Parse_Pattern;

end VSS.Regular_Expressions.ECMA_Parser;
