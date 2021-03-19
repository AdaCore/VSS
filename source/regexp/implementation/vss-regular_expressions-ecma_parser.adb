------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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
         Ok    : out Boolean);
      --  if Cursor points to Value, then forwart Cursor and set Ok to True,
      --  otherwise set Ok to False.

      procedure Alternative (Value : out Node; Ok : in out Boolean);
      procedure Disjunction (Value : out Node; Ok : in out Boolean);
      procedure Term (Value : out Node; Ok : in out Boolean);
      procedure Atom (Value : out Node; Ok : in out Boolean);

      procedure Alternative (Value : out Node; Ok : in out Boolean) is
         Right : Node;
      begin
         if Ok and Cursor.Has_Element then
            Term (Value, Ok);

            declare
               Save : constant Boolean := Ok;
            begin
               while Ok and Cursor.Has_Element loop
                  Term (Right, Ok);

                  if Ok then
                     Value := Create_Sequence (Value, Right);
                  end if;
               end loop;

               Ok := Save;
            end;
         elsif Ok then
            Value := Create_Empty;
         end if;
      end Alternative;

      procedure Atom (Value : out Node; Ok : in out Boolean) is
      begin
         case Cursor.Element is
            when '(' =>
               Expect ('(', Ok);
               Expect ('?', Ok);
               Expect (':', Ok);
               Disjunction (Value, Ok);
               Expect (')', Ok);
            when '^' | '$' | '\' | '.' | '*' | '+' | '?' | ')' |
               '[' | ']' | '{' | '}' | '|'
               =>
               Ok := False;
            when others =>
               Value := Create_Character (Cursor.Element);
               Expect (Cursor.Element, Ok);
         end case;
      end Atom;

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
         Ok    : out Boolean) is
      begin
         Ok :=  Cursor.Has_Element
           and then Cursor.Element = Value
           and then (Cursor.Forward or True);
      end Expect;

      procedure Term (Value : out Node; Ok : in out Boolean) is
      begin
         if not Ok then
            return;
         end if;

         Atom (Value, Ok);

         if Ok and then Cursor.Has_Element and then Cursor.Element = '*' then
            Expect (Cursor.Element, Ok);
            Value := Create_Star (Value);
         end if;
      end Term;

      Ok : Boolean := True;
   begin
      Disjunction (Result, Ok);

      if not Ok or Cursor.Has_Element then
         Error := "Unable to parse";
      end if;
   end Parse_Pattern;

end VSS.Regular_Expressions.ECMA_Parser;
