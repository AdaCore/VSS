--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters.Latin;

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

      Max_Nesting : constant Natural := 256;
      Depth : Natural := 0;

      procedure Expect
        (Value : VSS.Characters.Virtual_Character;
         Ok    : in out Boolean);
      --  if Cursor points to Value, then forward Cursor and set Ok to True,
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
      --  `[\p{L}\p{Nl}_]`. For this example first two character classes
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
               Category  : Node_Or_Class;
         end case;
      end record;

      --  Procedures for each syntax rule:

      procedure Alternative (Value : out Node_Or_Class; Ok : in out Boolean);
      procedure Disjunction (Value : out Node_Or_Class; Ok : in out Boolean);

      procedure Term (Value : out Node_Or_Class; Ok : in out Boolean)
        with Pre => Ok;

      procedure Atom_Or_Assertion
        (Value   : out Node_Or_Class;
         Is_Atom : out Boolean;
         Ok      : in out Boolean)
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
         Ok    : in out Boolean)
          with Pre => Ok;

      procedure Character_Class_Escape
        (Value : out Node_Or_Class; Ok : in out Boolean);

      procedure Character_Escape
        (Value : out VSS.Characters.Virtual_Character'Base;
         Ok    : in out Boolean)
          with Pre => Ok;

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

      -----------------
      -- Atom_Escape --
      -----------------

      procedure Atom_Escape (Value : out Node_Or_Class; Ok : in out Boolean) is
         Character : VSS.Characters.Virtual_Character'Base;

      begin
         if not Cursor.Has_Element then
            if Error.Is_Empty then
               Error := "Unexpected end of string in escape.";
            end if;

            Ok := False;
            return;
         end if;

         case Cursor.Element is
            when 'p' | 'P' | 'd' | 'D' | 's' | 'S' | 'w' | 'W' =>
               Character_Class_Escape (Value, Ok);

            when others =>
               Character_Escape (Character, Ok);

               if Ok then
                  Value := From_Node (Create_Character (Character));
               end if;

         end case;
      end Atom_Escape;

      procedure Atom_Or_Assertion
        (Value   : out Node_Or_Class;
         Is_Atom : out Boolean;
         Ok      : in out Boolean) is
      begin
         Is_Atom := True;

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
                     Value := From_Node
                       (Create_Group
                         (To_Node (Value), Group, Next_Group - 1));

                     Expect (')', Ok);
                  end;
               end if;
            when '[' =>
               Character_Class (Value, Ok);

            when ')' | '|' =>
               raise Program_Error;

            when '\' =>
               Expect ('\', Ok);

               if Cursor.Has_Element and then Cursor.Element in 'b' | 'B' then
                  Value := From_Node
                    (Create_Simple_Assertion
                      (if Cursor.Element = 'b' then Word_Boundary
                       else No_Word_Boundary));

                  Expect (Cursor.Element, Ok);

                  Is_Atom := False;

               else
                  Atom_Escape (Value, Ok);
               end if;

            when '^' | '$' | '*' | '+' | '?' | ']' | '{' | '}' =>
               if Error.Is_Empty then
                  Error := "Unexpected '";
                  Error.Append (Cursor.Element);
                  Error.Append ("' while atom expected.");
               end if;

               Ok := False;

            when '.' =>
               Expect ('.', Ok);
               Value := From_Node (Create_Any_Character);

            when others =>
               Value := From_Node (Create_Character (Cursor.Element));
               Expect (Cursor.Element, Ok);
         end case;
      end Atom_Or_Assertion;

      procedure Character_Class
        (Value : out Node_Or_Class; Ok : in out Boolean)
      is
         use type Name_Sets.General_Category_Set;

         Negate : Boolean := False;
      begin
         Expect ('[', Ok);

         if Ok and then Cursor.Has_Element and then Cursor.Element = '^' then
            Expect ('^', Ok);
            Negate := True;
         end if;

         Class_Ranges (Value, Ok);
         Expect (']', Ok);

         if Ok and Negate then
            case Value.Has_Node is
               when True =>
                  Value := From_Node (Create_Negated_Class (To_Node (Value)));
               when False =>
                  Value.Category := not Value.Category;
            end case;
         end if;
      end Character_Class;

      procedure Character_Class_Escape
        (Value : out Node_Or_Class; Ok : in out Boolean)
      is
         use type Name_Sets.General_Category_Set;

         Item   : Node;
         Negate : Boolean;
      begin
         if not Ok then
            return;
         elsif not Cursor.Has_Element then
            Ok := False;
            Error := "Unexpected end of string";
            return;
         end if;

         case Cursor.Element is
            when 'w' | 'W' =>  --  word characters
               Item := Create_Character_Range ('a', 'z');

               Item := Create_Alternative
                 (Item, Create_Character_Range ('A', 'Z'));

               Item := Create_Alternative
                 (Item, Create_Character_Range ('0', '9'));

               Item := Create_Alternative (Item, Create_Character ('_'));

               if Cursor.Element = 'W' then
                  Item := Create_Negated_Class (Item);
               end if;

               Value := From_Node (Item);
               Expect (Cursor.Element, Ok);
               return;

            when 's' | 'S' =>  --  Whitespace
               --  \r\n\t\f\v
               Item := Create_Character_Range
                 (VSS.Characters.Latin.Character_Tabulation,
                  VSS.Characters.Latin.Carriage_Return);

               --  Append zwnbsp
               Item := Create_Alternative
                 (Item, Create_Character
                    (VSS.Characters.Virtual_Character'Val (16#FEFF#)));

               --  Append any "z" (Space, Line and Paragraph Separator)
               Value := From_Node (Item);
               Name_Sets.To_General_Category_Set ("z", Value.Category, Ok);

               if Cursor.Element = 'S' then
                  Item := To_Node (Value);
                  Item := Create_Negated_Class (Item);
                  Value := From_Node (Item);
               end if;

               Expect (Cursor.Element, Ok);
               return;
            when 'd' | 'D' =>
               Item := Create_Character_Range ('0', '9');

               if Cursor.Element = 'D' then
                  Item := Create_Negated_Class (Item);
               end if;

               Value := From_Node (Item);
               Expect (Cursor.Element, Ok);
               return;
            when 'p' =>
               Negate := False;
            when 'P' =>
               Negate := True;
            when others =>
               Ok := False;
               Error := "Only \P or \p supported";
               return;
         end case;

         Expect (Cursor.Element, Ok);
         Expect ('{', Ok);
         Lone_Unicode_Property_Name_Or_Value (Value.Category, Ok);
         Expect ('}', Ok);

         if Ok and Negate then
            Value.Category := not Value.Category;
         end if;
      end Character_Class_Escape;

      ----------------------
      -- Character_Escape --
      ----------------------

      procedure Character_Escape
        (Value : out VSS.Characters.Virtual_Character'Base;
         Ok    : in out Boolean) is
      begin
         Value := VSS.Characters.Virtual_Character'Base'Last;

         case Cursor.Element is
            when '^' | '$' | '\' | '.' | '*' | '+' | '?' |
               '(' | ')' | '[' | ']' | '{' | '}' | '|'
               =>

               Value := Cursor.Element;

            when 'f' =>
               Value := VSS.Characters.Virtual_Character'Val (16#0C#);

            when 'n' =>
               Value := VSS.Characters.Virtual_Character'Val (16#0A#);

            when 'r' =>
               Value := VSS.Characters.Virtual_Character'Val (16#0D#);

            when 't' =>
               Value := VSS.Characters.Virtual_Character'Val (16#09#);

            when 'v' =>
               Value := VSS.Characters.Virtual_Character'Val (16#0B#);

            when 'c' =>
               Expect (Cursor.Element, Ok);

               if not Cursor.Has_Element then
                  Ok := False;
                  Error := "Incomplete \c escape sequence.";
                  return;

               elsif Cursor.Element in 'A' .. 'Z' | 'a' .. 'z' then
                  Value := VSS.Characters.Virtual_Character'Val
                    (VSS.Characters.Virtual_Character'Pos (Cursor.Element)
                     mod 32);

               else
                  Ok := False;
                  Error := "Unexpected character in \c escape sequence.";
                  return;

               end if;

            when others =>
               Ok := False;
               Error := "Unsupported escape sequence.";
               return;

         end case;

         Expect (Cursor.Element, Ok);
      end Character_Escape;

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
         Ok    : in out Boolean) is
      begin
         if not Cursor.Has_Element then
            Ok := False;
            Error := "Unexpected end of string";
            return;
         end if;

         case Cursor.Element is
            when 'p' | 'P' | 'd' | 'D' | 's' | 'S' | 'w' | 'W' =>
               Value := (Is_Character => False, Category => <>);
               Character_Class_Escape (Value.Category, Ok);

            when '-' =>
               Expect ('-', Ok);
               Value := (Is_Character => True, Character => '-');

            when others =>
               Value := (Is_Character => True, Character => <>);
               Character_Escape (Value.Character, Ok);
         end case;

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
               Right := To_Node (Item.Category);
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
         Depth := Depth + 1;
         if Depth = Max_Nesting then
            Error := "Pattern nesting too deep";
            Ok := False;
            return;
         end if;

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

         Depth := Depth - 1;
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
         Is_Atom : Boolean := False;
      begin
         if Cursor.Has_Element and then Cursor.Element in '^' | '$' then
            Value := From_Node
              (Create_Simple_Assertion
                (if Cursor.Element = '^' then Start_Of_Line else End_Of_Line));

            Expect (Cursor.Element, Ok);
         else
            Atom_Or_Assertion (Value, Is_Atom, Ok);
         end if;

         if Ok and then
           Cursor.Has_Element and then
           Cursor.Element in '*' | '+' | '?'
         then
            if Is_Atom then
               case Cursor.Element is
                  when '*' =>
                     Expect (Cursor.Element, Ok);
                     Value := From_Node (Create_Star (To_Node (Value)));

                  when '+' =>
                     Expect (Cursor.Element, Ok);
                     Value := From_Node (Create_Plus (To_Node (Value)));

                  when '?' =>
                     Expect (Cursor.Element, Ok);
                     Value := From_Node
                       (Create_Alternative (To_Node (Value), Create_Empty));

                  when others =>
                     null;
               end case;
            else
               Ok := False;
               Error := "The assertion is not quantifiable";
            end if;
         end if;
      end Term;

      function To_Node (Value : Node_Or_Class) return Node is
         use type Name_Sets.General_Category_Set;

         function Left return Node is
           (Create_General_Category_Set (Value.Category));
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
