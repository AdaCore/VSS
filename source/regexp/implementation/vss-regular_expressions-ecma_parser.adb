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

      type Node_Or_Class (Has_Node : Boolean := False) is record
         Category : Name_Sets.General_Category_Set;

         case Has_Node is
            when True =>
               Node : ECMA_Parser.Node;
            when False =>
               null;
         end case;
      end record;
      --  This type represent Node or/and Category_Set. It separates special
      --  character class nodes into a dedicated field to optimize regexp like
      --  `[\p{L}\p{Nl}_]`. For this expample first two character classes
      --  populates Category filed and '_' populates Node field. Any of field
      --  could be empty.

      function To_Node (Value : Node_Or_Class) return Node;
      --  Create a Node from Node_Or_Class

      function From_Node (Value : Node) return Node_Or_Class is
        (Has_Node => True,
         Category => Name_Sets.Empty,
         Node     => Value);
      --  Create Node_Or_Class from a Node

      function "or" (Left, Right : Node_Or_Class) return Node_Or_Class;
      --  Return (left|right)

      type Character_Or_Set (Is_Character : Boolean := False) is record
         case Is_Character is
            when True =>
               Character : VSS.Characters.Virtual_Character;
            when False =>
               Category  : Name_Sets.General_Category_Set;
         end case;
      end record;

      --  Procedures for each syntax rule:

      procedure Alternative (Value : out Node_Or_Class; Ok : in out Boolean);
      procedure Disjunction (Value : out Node_Or_Class; Ok : in out Boolean);

      procedure Term (Value : out Node_Or_Class; Ok : in out Boolean)
        with Pre => Ok;

      procedure Atom (Value : out Node_Or_Class; Ok : in out Boolean)
        with Pre => Ok;

      procedure Atom_Escape (Value : out Node_Or_Class; Ok : in out Boolean)
        with Pre => Ok;

      procedure Unicode_Property_Value_Characters
        (Value : out VSS.Strings.Virtual_String; Ok : in out Boolean);

      procedure Lone_Unicode_Property_Name_Or_Value
        (Value : out Name_Sets.General_Category_Set; Ok : in out Boolean);

      procedure Character_Class
        (Value : out Node_Or_Class; Ok : in out Boolean)
          with Pre => Ok;

      procedure Class_Ranges (Value : out Node_Or_Class; Ok : in out Boolean);

      procedure Class_Atom
        (Value : out Character_Or_Set;
         Ok    : in out Boolean)
        with Pre => Ok
          and then Cursor.Has_Element
          and then Cursor.Element /= ']';

      procedure Class_Escape
        (Value : out Character_Or_Set;
         Ok    : in out Boolean);

      procedure Character_Class_Escape
        (Value : out Name_Sets.General_Category_Set; Ok : in out Boolean);

      Next_Group : Positive := 1;  --  Group counter

      --  Implementations

      function "or" (Left, Right : Node_Or_Class) return Node_Or_Class is
         use type Name_Sets.General_Category_Set;

      begin
         if Left.Has_Node then
            if Right.Has_Node then
               return (Has_Node => True,
                       Category => Left.Category or Right.Category,
                       Node     => Create_Alternative (Left.Node, Right.Node));
            else
               return (Has_Node => True,
                       Category => Left.Category or Right.Category,
                       Node     => Left.Node);
            end if;
         elsif Right.Has_Node then
            return (Has_Node => True,
                    Category => Left.Category or Right.Category,
                    Node     => Right.Node);
         else
            return (Has_Node => False,
                    Category => Left.Category or Right.Category);
         end if;
      end "or";

      procedure Alternative (Value : out Node_Or_Class; Ok : in out Boolean) is
         Right : Node_Or_Class;
      begin
         if Ok and (not Cursor.Has_Element
                    or else Cursor.Element in ')' | '|')
         then
            Value := From_Node (Create_Empty);

         elsif Ok then
            Term (Value, Ok);

            while Ok
              and then Cursor.Has_Element
              and then Cursor.Element not in ')' | '|'
            loop
               Term (Right, Ok);

               if Ok then
                  Value := From_Node
                    (Create_Sequence (To_Node (Value), To_Node (Right)));
               end if;
            end loop;
         end if;
      end Alternative;

      procedure Atom (Value : out Node_Or_Class; Ok : in out Boolean) is
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
                     Value :=
                       From_Node (Create_Group (To_Node (Value), Group));
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
               Value := From_Node (Create_Character (Cursor.Element));
               Expect (Cursor.Element, Ok);
         end case;
      end Atom;

      procedure Atom_Escape (Value : out Node_Or_Class; Ok : in out Boolean) is
         Set : Name_Sets.General_Category_Set;
      begin
         Character_Class_Escape (Set, Ok);

         if Ok then
            Value := (Has_Node => False, Category => Set);
         end if;
      end Atom_Escape;

      procedure Character_Class
        (Value : out Node_Or_Class; Ok : in out Boolean) is
      begin
         Expect ('[', Ok);
         Class_Ranges (Value, Ok);
         Expect (']', Ok);
      end Character_Class;

      procedure Character_Class_Escape
        (Value : out Name_Sets.General_Category_Set; Ok : in out Boolean) is
      begin
         Expect ('p', Ok);
         Expect ('{', Ok);
         Lone_Unicode_Property_Name_Or_Value (Value, Ok);
         Expect ('}', Ok);
      end Character_Class_Escape;

      procedure Class_Atom
        (Value : out Character_Or_Set;
         Ok    : in out Boolean) is
      begin
         if Cursor.Element = '\' then
            Expect ('\', Ok);
            Class_Escape (Value, Ok);
         else
            Value := (Is_Character => True, Character => Cursor.Element);
            Expect (Cursor.Element, Ok);
         end if;
      end Class_Atom;

      procedure Class_Escape
        (Value : out Character_Or_Set;
         Ok    : in out Boolean)
      is
         Set : Name_Sets.General_Category_Set;
      begin
         Character_Class_Escape (Set, Ok);

         if Ok then
            Value := (Is_Character => False, Category => Set);
         end if;
      end Class_Escape;

      procedure Class_Ranges
        (Value : out Node_Or_Class; Ok : in out Boolean)
      is
         function No_Close_Bracket return VSS.Strings.Virtual_String is
            ("No ']' found");

         procedure Append
           (Item  : Character_Or_Set;
            First : in out Boolean);
         --  Convert Item to Node and append it to Value

         procedure Append_Node
           (Right : Node;
            First : in out Boolean);
         --  If First replace Value with a node, otherwise append it to Value

         ------------
         -- Append --
         ------------

         procedure Append
           (Item  : Character_Or_Set;
            First : in out Boolean)
         is
            Right : Node;
         begin
            if Item.Is_Character then
               Right := Create_Character (Item.Character);
            else
               Right := Create_General_Category_Set (Item.Category);
            end if;

            Append_Node (Right, First);
         end Append;

         -----------------
         -- Append_Node --
         -----------------

         procedure Append_Node
           (Right : Node;
            First : in out Boolean) is
         begin
            if First then
               First := False;
               Value := From_Node (Right);
            else
               Value := Value or From_Node (Right);
            end if;
         end Append_Node;

         From  : Character_Or_Set;
         To    : Character_Or_Set;
         First : Boolean := True;
      begin
         if Ok and then Cursor.Has_Element and then Cursor.Element = ']' then
            --  An empty character class
            Value := (Has_Node => False, Category => Name_Sets.Empty);
            return;
         elsif Ok and then not Cursor.Has_Element then
            Error := No_Close_Bracket;
            Ok := False;
         end if;

         while Ok
           and then Cursor.Has_Element
           and then Cursor.Element /= ']'
         loop
            Class_Atom (From, Ok);

            exit when not Ok;

            if not Cursor.Has_Element then
               Error := No_Close_Bracket;
               Ok := False;
            elsif Cursor.Element = '-' then
               Expect ('-', Ok);

               if not Cursor.Has_Element then
                  Error := No_Close_Bracket;
                  Ok := False;
               elsif Cursor.Element = ']' then  --  like [...a-]
                  Append (From, First);
                  Append ((True, '-'), First);
               else  --  like [...a-z...]
                  Class_Atom (To, Ok);

                  exit when not Ok;

                  if From.Is_Character and To.Is_Character then
                     Append_Node
                       (Create_Character_Range (From.Character, To.Character),
                        First);
                  else
                     Ok := False;
                     Error := "Range boundary can't be a character class";
                  end if;
               end if;
            else  --  like [...a...]
               Append (From, First);
            end if;
         end loop;
      end Class_Ranges;

      procedure Disjunction (Value : out Node_Or_Class; Ok : in out Boolean) is
         Right : Node_Or_Class;
      begin
         Alternative (Value, Ok);

         while Ok and then
           Cursor.Has_Element and then
           Cursor.Element /= ')'
         loop
            Expect ('|', Ok);
            Alternative (Right, Ok);

            if Ok then
               Value := Value or Right;
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
        (Value : out Name_Sets.General_Category_Set; Ok : in out Boolean)
      is
         Name : VSS.Strings.Virtual_String;
      begin
         Unicode_Property_Value_Characters (Name, Ok);

         if Ok then
            Name_Sets.To_General_Category_Set (Name, Value, Ok);
         end if;
      end Lone_Unicode_Property_Name_Or_Value;

      procedure Term (Value : out Node_Or_Class; Ok : in out Boolean) is
      begin
         Atom (Value, Ok);

         if Ok and then Cursor.Has_Element and then Cursor.Element = '*' then
            Expect (Cursor.Element, Ok);
            Value := From_Node (Create_Star (To_Node (Value)));
         end if;
      end Term;

      function To_Node (Value : Node_Or_Class) return Node is
         use type Name_Sets.General_Category_Set;

         function Left return Node is
           (if Value.Category = Name_Sets.Empty then Create_Empty
            else Create_General_Category_Set (Value.Category));
      begin
         if not Value.Has_Node then
            return Left;
         elsif Value.Category = Name_Sets.Empty then
            return Value.Node;
         else
            return Create_Alternative (Left, Value.Node);
         end if;
      end To_Node;

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

      Ok    : Boolean := True;
      Value : Node_Or_Class;
   begin
      Disjunction (Value, Ok);
      Result := To_Node (Value);

      if not Error.Is_Empty then
         null;
      elsif not Ok or Cursor.Has_Element then
         Error := "Unable to parse";
      end if;
   end Parse_Pattern;

end VSS.Regular_Expressions.ECMA_Parser;
