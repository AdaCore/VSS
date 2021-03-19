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

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with VSS.Regular_Expressions.ECMA_Parser;
with VSS.Regular_Expressions.Matches;
with VSS.Strings.Character_Iterators;

package body VSS.Regular_Expressions.DFA_Engines is

   ----------
   -- Hash --
   ----------

   function Hash (Value : Jump) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type'Mod (Value.State) * 7919
        + VSS.Characters.Virtual_Character'Pos (Value.Char);
   end Hash;

   -----------
   -- Match --
   -----------

   overriding procedure Match
     (Self    : Engine;
      Subject : VSS.Strings.Virtual_String;
      Options : Match_Options := No_Match_Options;
      Result  : out Match_Access)
   is
      State  : Integer := 1;
      Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
        Subject.First_Character;
   begin
      if Cursor.Has_Element then
         loop
            declare
               Char : constant VSS.Characters.Virtual_Character :=
                 Cursor.Element;

               Next : constant Jump_Maps.Cursor :=
                 Self.Jumps.Find ((Char, abs State));
            begin
               if Jump_Maps.Has_Element (Next) then
                  State := Jump_Maps.Element (Next);
               else
                  State := 0;

                  exit;
               end if;
            end;

            exit when not Cursor.Forward;
         end loop;
      end if;

      Result := new VSS.Regular_Expressions.Matches.Match;
      Result.Has_Match := State < 0;
   end Match;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Self    : in out Engine;
      Pattern : VSS.Strings.Virtual_String;
      Options : VSS.Regular_Expressions.Pattern_Options;
      Fit     : out Boolean)
   is
      type Node_Kind is (Character, Sequence, Alternative, Star, Empty);
      type Node_Position is new Positive;
      type Position_Set is array (Node_Position range <>) of Boolean with Pack;

      function Union (Left, Right : Position_Set) return Position_Set;
      function Expand
        (Set   : Position_Set;
         First : Node_Position;
         Last  : Node_Position'Base) return Position_Set
      with Post => Expand'Result'First = First and Expand'Result'Last = Last;

      type Node;
      type Node_Access is access Node;
      type Node
        (Kind : Node_Kind;
         From : Node_Position;
         To   : Node_Position'Base) is
      record
         Next : Node_Access;  --  Deallocation list
         case Kind is
            when Character =>
               Position : Node_Position;
            when Sequence | Alternative =>
               Left, Right : not null Node_Access;
               Nullable    : Boolean;
               First_Pos   : Position_Set (From .. To);
               Last_Pos    : Position_Set (From .. To);
            when Star =>
               Child       : not null Node_Access;
            when Empty =>
               null;
         end case;
      end record;

      function Create_Character (Value : VSS.Characters.Virtual_Character)
        return Node_Access;

      function Create_Sequence (Left, Right : not null Node_Access)
        return Node_Access;

      function Create_Alternative (Left, Right : Node_Access)
        return Node_Access;

      function Create_Star (Left : Node_Access) return Node_Access;

      function Create_Empty return Node_Access;

      function Is_Nullable (Self : not null Node_Access) return Boolean;
      function First_Pos (Self : not null Node_Access) return Position_Set;
      function Last_Pos (Self : not null Node_Access) return Position_Set;

      procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

      List  : Node_Access;  --  List of nodes to be deallocated
      Last  : Node_Position'Base := 0;
      Chars : array (1 .. Node_Position (Pattern.Character_Length) + 1) of
        VSS.Characters.Virtual_Character;

      function Create_Character (Value : VSS.Characters.Virtual_Character)
        return Node_Access is
      begin
         Last := Last + 1;
         Chars (Last) := Value;

         List := new Node'(Character, 1, 0, List, Last);
         return List;
      end Create_Character;

      function Create_Sequence (Left, Right : not null Node_Access)
        return Node_Access
      is
         Left_Nullable  : constant Boolean := Is_Nullable (Left);
         Right_Nullable : constant Boolean := Is_Nullable (Right);

         First : constant Position_Set :=
           (if Left_Nullable
            then Union (First_Pos (Left), First_Pos (Right))
            else First_Pos (Left));

         Last : constant Position_Set :=
           (if Right_Nullable
            then Union (Last_Pos (Left), Last_Pos (Right))
            else Last_Pos (Right));

         From : constant Node_Position := Node_Position'Min
           (First'First, Last'First);

         To   : constant Node_Position'Base := Node_Position'Max
           (First'Last, Last'Last);
      begin
         List := new Node'
           (Kind      => Sequence,
            From      => From,
            To        => To,
            Next      => List,
            Left      => Left,
            Right     => Right,
            Nullable  => Left_Nullable and Right_Nullable,
            First_Pos => Expand (First, From, To),
            Last_Pos  => Expand (Last, From, To));

         return List;
      end Create_Sequence;

      function Create_Star (Left : Node_Access) return Node_Access is
      begin
         List := new Node'
           (Kind      => Star,
            From      => 1,
            To        => 0,
            Next      => List,
            Child     => Left);

         return List;
      end Create_Star;

      function Create_Alternative (Left, Right : Node_Access)
        return Node_Access
      is
         Left_Nullable  : constant Boolean := Is_Nullable (Left);
         Right_Nullable : constant Boolean := Is_Nullable (Right);

         First : constant Position_Set :=
           Union (First_Pos (Left), First_Pos (Right));

         Last : constant Position_Set :=
           Union (Last_Pos (Left), Last_Pos (Right));

         From : constant Node_Position := Node_Position'Min
           (First'First, Last'First);

         To   : constant Node_Position'Base := Node_Position'Max
           (First'Last, Last'Last);
      begin
         List := new Node'
           (Kind      => Alternative,
            From      => From,
            To        => To,
            Next      => List,
            Left      => Left,
            Right     => Right,
            Nullable  => Left_Nullable or Right_Nullable,
            First_Pos => Expand (First, From, To),
            Last_Pos  => Expand (Last, From, To));

         return List;
      end Create_Alternative;

      function Create_Empty return Node_Access is
      begin
         List := new Node'(Empty, 1, 0, List);
         return List;
      end Create_Empty;

      package Parser is new VSS.Regular_Expressions.ECMA_Parser (Node_Access);

      function Expand
        (Set   : Position_Set;
         First : Node_Position;
         Last  : Node_Position'Base) return Position_Set is
      begin
         return (First .. Set'First - 1 => False) &
                 Set &
                (Set'Last + 1 .. Last => False);
      end Expand;

      function First_Pos (Self : not null Node_Access) return Position_Set is
      begin
         case Self.Kind is
            when Character =>
               return (Self.Position .. Self.Position => True);
            when Sequence | Alternative =>
               return Self.First_Pos;
            when Star =>
               return First_Pos (Self.Child);
            when Empty =>
               return (1 .. 0 => False);
         end case;
      end First_Pos;

      function Is_Nullable (Self : not null Node_Access) return Boolean is
      begin
         case Self.Kind is
            when Character =>
               return False;
            when Sequence | Alternative =>
               return Self.Nullable;
            when Empty | Star =>
               return True;
         end case;
      end Is_Nullable;

      function Last_Pos (Self : not null Node_Access) return Position_Set is
      begin
         case Self.Kind is
            when Character =>
               return (Self.Position .. Self.Position => True);
            when Sequence | Alternative =>
               return Self.Last_Pos;
            when Star =>
               return Last_Pos (Self.Child);
            when Empty =>
               return (1 .. 0 => False);
         end case;
      end Last_Pos;

      function Union (Left, Right : Position_Set) return Position_Set is
      begin
         if Left'Length = 0 then
            return Right;
         elsif Right'Length = 0 then
            return Left;
         else
            declare
               From : constant Node_Position :=
                 Node_Position'Min (Left'First, Right'First);
               To   : constant Node_Position'Base :=
                 Node_Position'Max (Left'Last, Right'Last);
               L    : constant Position_Set (From .. To) :=
                 Expand (Left, From, To);
               R    : constant Position_Set (From .. To) :=
                 Expand (Right, From, To);
            begin
               return L or R;
            end;
         end if;
      end Union;

      Error  : VSS.Strings.Virtual_String;
      Root   : Node_Access;
      Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
        Pattern.First_Character;
   begin
      Parser.Parse_Pattern
        (Cursor => Cursor,
         Error  => Error,
         Result => Root);

      Root := Create_Sequence (Root, Create_Character (' '));

      declare
         subtype Set is Position_Set (1 .. Last);

         function Hash (Value : Set) return Ada.Containers.Hash_Type;

         function Hash (Value : Set) return Ada.Containers.Hash_Type is
            use type Ada.Containers.Hash_Type;

            Result : Ada.Containers.Hash_Type := 0;
         begin
            for J of Value loop
               Result := Result * 2
                 xor Boolean'Pos (J)
                 xor Boolean'Pos (Result > 2**31);
            end loop;

            return Result;
         end Hash;

         Follow_Pos : array (1 .. Last) of Set :=
           (others => (others => False));
         --  Follow_Pos (J) denotes positions following J

         procedure Fill (Self : Node_Access);

         procedure Fill (Self : Node_Access) is
         begin
            case Self.Kind is
               when Sequence =>
                  declare
                     RF : constant Position_Set := First_Pos (Self.Right);
                     LL : constant Position_Set := Last_Pos (Self.Left);
                  begin
                     for J in LL'Range loop
                        if LL (J) then
                           Follow_Pos (J) (RF'First .. RF'Last) :=
                             Follow_Pos (J) (RF'First .. RF'Last) or RF;
                        end if;
                     end loop;

                     Fill (Self.Left);
                     Fill (Self.Right);
                  end;
               when Alternative =>
                  Fill (Self.Left);
                  Fill (Self.Right);
                  return;
               when Character | Empty =>
                  return;
               when Star =>
                  declare
                     F : constant Position_Set := First_Pos (Self.Child);
                     L : constant Position_Set := Last_Pos (Self.Child);
                  begin
                     for J in L'Range loop
                        if L (J) then
                           Follow_Pos (J) (F'First .. F'Last) :=
                             Follow_Pos (J) (F'First .. F'Last) or F;
                        end if;
                     end loop;

                     Fill (Self.Child);
                  end;
            end case;
         end Fill;

         package State_Maps is new Ada.Containers.Hashed_Maps
           (Key_Type        => Set,
            Element_Type    => Natural,
            Hash            => Hash,
            Equivalent_Keys => "=");

         package State_Lists is new Ada.Containers.Vectors (Positive, Set);

         States : State_Maps.Map;
         Marks  : State_Lists.Vector;
      begin
         Fill (Root);

         Marks.Append (Expand (First_Pos (Root), 1, Last));
         States.Insert (Marks.First_Element, 1);

         while not Marks.Is_Empty loop
            declare
               T    : constant Set := Marks.Last_Element;
               Skip : Set := (others => False);  --  Already processed in T
               Next : Integer;  --  Next state code
            begin
               Skip (Last) := True;
               Marks.Delete_Last;

               for P in T'Range loop
                  if T (P) and not Skip (P) then
                     declare
                        use type VSS.Characters.Virtual_Character;
                        U      : Set := (others => False);
                        Cursor : State_Maps.Cursor;
                        Ok     : Boolean := False;
                        Char   : constant VSS.Characters.Virtual_Character :=
                          Chars (P);
                     begin
                        for J in P .. T'Last loop
                           if T (J) and Chars (J) = Char then
                              Skip (J) := True;
                              U := U or Follow_Pos (J);
                           end if;
                        end loop;

                        if U /= (Set'Range => False) then
                           States.Insert
                             (U, Natural (States.Length) + 1, Cursor, Ok);

                           Next := State_Maps.Element (Cursor);

                           if U (Last) then
                              Next := -Next;  --  Mark as a finish state
                           end if;

                           Self.Jumps.Insert ((Char, States (T)), Next);
                        end if;

                        if Ok then
                           Marks.Append (U);
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end;

      Self.Initialize (Options, Error, Pattern);

      while List /= null loop
         declare
            Next : constant Node_Access := List.Next;
         begin
            Free (List);
            List := Next;
         end;
      end loop;

      Fit := True;
   end Parse;

   ----------------
   -- On_Destroy --
   ----------------

   overriding procedure On_Destroy (Self : in out Engine) is
   begin
      null;
   end On_Destroy;

end VSS.Regular_Expressions.DFA_Engines;
