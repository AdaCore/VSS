--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with VSS.Characters.Latin;
with VSS.Characters.Punctuations;
with VSS.Implementation.Text_Handlers;
with VSS.Implementation.Strings;
with VSS.Regular_Expressions.ECMA_Parser;
with VSS.Regular_Expressions.Matches;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Cursors.Internals;
with VSS.Strings.Cursors.Markers.Internals;
with VSS.Strings.Internals;
with VSS.Unicode;

package body VSS.Regular_Expressions.Pike_Engines is

   pragma Suppress (Container_Checks);

   package Instruction_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Instruction_Address,
      Element_Type => Instruction);

   package Address_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Instruction_Address);
   --  List of instruction addresses

   -------------------------
   -- Capture_Group_Count --
   -------------------------

   overriding function Capture_Group_Count (Self : Engine) return Natural is
   begin
      return Natural (Self.Last_Tag / 2) - 1;
   end Capture_Group_Count;

   -----------
   -- Match --
   -----------

   overriding procedure Match
     (Self    : Engine;
      Subject : VSS.Strings.Virtual_String;
      From    : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Options : Match_Options := No_Match_Options;
      Result  : out Match_Access)
   is
      use type VSS.Characters.Virtual_Character;

      type Uninitialized_Cursor is record
         Index        : VSS.Implementation.Strings.Character_Count;
         UTF8_Offset  : VSS.Unicode.UTF8_Code_Unit_Offset;
         UTF16_Offset : VSS.Unicode.UTF16_Code_Unit_Offset;
      end record;
      --  This is the same record as VSS.Implementation.Strings.Cursor, but
      --  without initialization. It helps when we preallocate thread states
      --  because it could contain a lot of cursors.

      type Tag_Array is array (1 .. Self.Last_Tag) of
        Uninitialized_Cursor;
      --  First two items represent whole match boundaries, while others
      --  represent groups boundaries.
      --  Odd items represent left boundaries, even items - right.
      --  Even items point to next character after the bound.

      Invalid_Cursor : constant VSS.Implementation.Strings.Cursor :=
        (others => <>);
      --  Cursor pointing nowhere.

      Invalid : constant Uninitialized_Cursor :=
        (Index        => Invalid_Cursor.Index,
         UTF8_Offset  => Invalid_Cursor.UTF8_Offset,
         UTF16_Offset => Invalid_Cursor.UTF16_Offset);
      --  The Invalid_Cursor value converted to Uninitialized_Cursor type

      Unset_Tags : constant Tag_Array := [1 .. Self.Last_Tag => Invalid];
      --  All unset tags have invalid values by default

      type Thread_State is record
         PC   : Instruction_Address;
         Tags : Tag_Array;
      end record;
      --  State of one execution thread has next instruction address and known
      --  tags (regexp subgroup boundaries).

      type Thread_State_Array is array (Positive range <>) of Thread_State;

      type Thread_State_List is record
         List : Thread_State_Array (1 .. Self.Max_Threads);
         Last : Natural := 0;  --  Last assigned item in the List
      end record;

      type Thread_State_List_Index is range 0 .. 1;
      --  We keep only two state lists, current and next
      type Thread_State_List_Pair is
        array (Thread_State_List_Index) of Thread_State_List;
      --  Active threads (for the current position) and threads to run in the
      --  next position.

      procedure Step_Backward
        (Text : VSS.Strings.Virtual_String'Class;
         Cursor : in out VSS.Implementation.Strings.Cursor);
      --  Shift Cursor one character backward in string Text

      procedure Append_State
        (Prev     : VSS.Characters.Virtual_Character;
         New_Tags : Tag_Array;
         From     : Instruction_Address);
      --  This procedure finds all states reachable from the given PC (From) by
      --  non-character instructions and appends new thread states to the
      --  next state list. Cursor points to a next unprocessed character
      --  if any. Prev is a previous character if Cursor isn't the first
      --  character of the subject.

      function Character_In_Class
        (PC   : Instruction_Address;
         Char : VSS.Characters.Virtual_Character) return Boolean;
      --  Check if Char in the class defined by a program started from PC.
      --  Program should contain only Split and character instructions.

      procedure Loop_Over_Characters
        (Cursor : in out VSS.Strings.Character_Iterators.Character_Iterator);
      --  Iterate over all characters of the Subject using Cursor

      procedure Append_One_State (Value : Thread_State);
      --  Append one Thread_State to next state list.

      States     : Thread_State_List_Pair;
      Current    : Thread_State_List_Index := 0;
      --  Index of the current state list in States
      function Previous return Thread_State_List_Index is (1 - Current);
      --  Index of the previous/next state list in States

      Found      : Boolean := False;
      --  Some match has been found
      Final_Tags : Tag_Array;
      --  If a match has been found, corresponding tag values

      Program : Instruction_Array renames Self.Program.all;

      Step  : Natural := 1;
      Steps : array (Program'Range) of Natural := [others => 0];
      --  A filter to protect the Next from duplicated states

      Cursor : VSS.Strings.Character_Iterators.Character_Iterator;

      Pos : constant not null
        VSS.Strings.Cursors.Internals.Cursor_Constant_Access :=
          VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant (Cursor);

      ----------------------
      -- Append_One_State --
      ----------------------

      procedure Append_One_State (Value : Thread_State) is
         Next : Thread_State_List renames States (Previous);
      begin
         Next.Last := @ + 1;
         Next.List (Next.Last) := Value;
      end Append_One_State;

      ------------------
      -- Append_State --
      ------------------

      procedure Append_State
        (Prev     : VSS.Characters.Virtual_Character;
         New_Tags : Tag_Array;
         From     : Instruction_Address)
      is
         function Is_Valid_Assertion (Kind : Simple_Assertion_Kind)
           return Boolean;
         --  Check given assertion

         function Is_Word_Char (Char : VSS.Characters.Virtual_Character)
           return Boolean is (Char in 'A' .. 'Z' | 'a' .. 'z' | '_');

         ------------------------
         -- Is_Valid_Assertion --
         ------------------------

         function Is_Valid_Assertion (Kind : Simple_Assertion_Kind)
           return Boolean
         is
            use type VSS.Strings.Character_Index;
         begin
            case Kind is
               when Start_Of_Line =>
                  return Cursor.Character_Index
                           = Match.From.First_Character_Index;
               when End_Of_Line =>
                  return not Cursor.Has_Element;
               when Word_Boundary =>
                  return (Cursor.Character_Index > 1 and then
                           Is_Word_Char (Prev))
                    xor (Cursor.Has_Element and then
                           Is_Word_Char (Cursor.Element));
               when No_Word_Boundary =>
                  return not Is_Valid_Assertion (Word_Boundary);
            end case;
         end Is_Valid_Assertion;

         PC   : Instruction_Address := From;
         Code : Instruction;

      begin
         while Steps (PC) /= Step loop --  Skip already processed PC

            Steps (PC) := Step;
            Code := Self.Program (PC);

            case Code.Kind is
               when Character | Class | Category | Negate_Class | Match =>
                  Append_One_State (Thread_State'(PC, New_Tags));
                  exit;

               when Split =>
                  Append_State (Prev, New_Tags, PC + Code.Next);
                  PC := PC + Code.Fallback;

               when Save =>
                  declare
                     Updated : Tag_Array := New_Tags;

                  begin
                     Updated (Code.Tag) :=
                       (Index        => Pos.Index,
                        UTF8_Offset  => Pos.UTF8_Offset,
                        UTF16_Offset => Pos.UTF16_Offset);

                     --  Reset nested subgroup tags if any:
                     Updated (Code.Tag + 1 .. Code.Last) :=
                       [others => Invalid];

                     Append_State (Prev, Updated, PC + Code.Next);
                     exit;
                  end;

               when Assertion =>
                  exit when not Is_Valid_Assertion (Code.Assertion);
                  PC := PC + Code.Next;

               when No_Operation =>
                  PC := PC + Code.Next;
            end case;
         end loop;
      end Append_State;

      ------------------------
      -- Character_In_Class --
      ------------------------

      function Character_In_Class
        (PC   : Instruction_Address;
         Char : VSS.Characters.Virtual_Character) return Boolean
      is
         Code : constant Instruction := Self.Program (PC);
      begin
         case Code.Kind is
            when Character =>
               return Code.Character = Char;

            when Class =>
               return Char in Code.From .. Code.To;

            when Category =>
               return Name_Sets.Contains
                 (Code.Category,
                  VSS.Characters.Get_General_Category (Char));

            when Split =>
               return Character_In_Class (PC + Code.Next, Char)
                 or else Character_In_Class (PC + Code.Fallback, Char);

            when others =>
               raise Program_Error with "Unexpected code in char class";
         end case;
      end Character_In_Class;

      --------------------------
      -- Loop_Over_Characters --
      --------------------------

      procedure Loop_Over_Characters
        (Cursor : in out VSS.Strings.Character_Iterators.Character_Iterator) is
      begin
         loop
            declare
               Char : constant VSS.Characters.Virtual_Character :=
                 Cursor.Element;

               --  Shift Cursor, so it points to the next character after Char
               Again : constant Boolean := Cursor.Forward;
            begin
               --  Swap state lists
               States (Current).Last := 0;
               Current := Previous;
               Step := Step + 1;

               --  Run each active thread
               for X of States (Current).List (1 .. States (Current).Last) loop
                  declare
                     Code : constant Instruction := Self.Program (X.PC);
                     Next : constant Instruction_Address := X.PC + Code.Next;
                  begin
                     case Code.Kind is
                        when Character =>
                           if Code.Character = Char then
                              Append_State (Char, X.Tags, Next);
                           end if;

                        when Class =>
                           if Char in Code.From .. Code.To then
                              Append_State (Char, X.Tags, Next);
                           end if;

                        when Category =>
                           if Name_Sets.Contains
                               (Code.Category,
                                VSS.Characters.Get_General_Category (Char))
                           then
                              Append_State (Char, X.Tags, Next);
                           end if;

                        when Negate_Class =>
                           if not Character_In_Class (X.PC + 1, Char) then
                              Append_State (Char, X.Tags, Next);
                           end if;

                        when Match =>
                           Found := True;
                           Final_Tags := X.Tags;

                           exit;
                        when others =>
                           raise Program_Error;
                     end case;
                  end;
               end loop;

               exit when not Again
                 or (States (Previous).Last = 0 and Found);

               if not Found and not Options (Anchored_Match) then
                  --  Start new thread from the current character if we haven't
                  --  found any match yet nor we are in anchored match.
                  Append_State (Char, Unset_Tags, 1);
               end if;
            end;
         end loop;
      end Loop_Over_Characters;

      -------------------
      -- Step_Backward --
      -------------------

      procedure Step_Backward
        (Text   : VSS.Strings.Virtual_String'Class;
         Cursor : in out VSS.Implementation.Strings.Cursor)
      is
         Ignore  : Boolean;

         Data    : constant
           VSS.Strings.Internals.String_Data_Constant_Access :=
             VSS.Strings.Internals.Data_Access_Constant (Text);
         Handler : constant
           VSS.Implementation.Strings.Constant_Text_Handler_Access :=
             VSS.Implementation.Strings.Constant_Handler (Data.all);

      begin
         if VSS.Implementation.Strings.Is_Invalid (Cursor) then
            null;

         else
            Ignore := Handler.Backward (Data.all, Cursor);
         end if;
      end Step_Backward;

      use type VSS.Strings.Character_Count;

   begin
      Cursor.Set_At (From.First_Marker);

      --  Start a new thread at the beginning of the sample
      Append_State (' ', Unset_Tags, 1);

      if Cursor.Has_Element then
         Loop_Over_Characters (Cursor);
      end if;

      Current := Previous;

      --  Find a reached final state
      for X of States (Current).List (1 .. States (Current).Last) loop
         declare
            Code : constant Instruction := Self.Program (X.PC);
         begin
            case Code.Kind is
               when Match =>
                  Found := True;
                  Final_Tags := X.Tags;

                  exit;
               when others =>
                  null;
            end case;
         end;
      end loop;

      if Found and then
        (not Options (Anchored_Match) or else
         VSS.Strings.Character_Count (Final_Tags (2).Index) =
            Subject.After_Last_Character.Character_Index)
      then
         Result := new VSS.Regular_Expressions.Matches.Match
           (Length => Natural (Self.Last_Tag / 2));
         Result.Has_Match := True;

         declare
            Index : Tag_Number := Final_Tags'First;
         begin
            Result.Connect (Subject);

            for J in Result.Markers'Range loop
               declare
                  From : constant VSS.Implementation.Strings.Cursor :=
                    (Index        => Final_Tags (Index).Index,
                     UTF8_Offset  => Final_Tags (Index).UTF8_Offset,
                     UTF16_Offset => Final_Tags (Index).UTF16_Offset);

                  To   : VSS.Implementation.Strings.Cursor :=
                    (Index        => Final_Tags (Index + 1).Index,
                     UTF8_Offset  => Final_Tags (Index + 1).UTF8_Offset,
                     UTF16_Offset => Final_Tags (Index + 1).UTF16_Offset);
               begin
                  Step_Backward (Result.Get_Owner.all, To);

                  Result.Markers (J) :=
                    VSS.Strings.Cursors.Markers.Internals.New_Segment_Marker
                      (Result.Owner.all,
                       First => From,
                       Last  => To);

                  Index := Index + 2;
               end;
            end loop;
         end;
      else
         Result := new VSS.Regular_Expressions.Matches.Match (Length => 0);
         Result.Has_Match := False;
      end if;
   end Match;

   ----------------
   -- On_Destroy --
   ----------------

   overriding procedure On_Destroy (Self : in out Engine) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Instruction_Array, Instruction_Array_Access);
   begin
      Free (Self.Program);
   end On_Destroy;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Self    : in out Engine;
      Pattern : VSS.Strings.Virtual_String;
      Options : VSS.Regular_Expressions.Pattern_Options;
      Fit     : out Boolean)
   is
      use type Instruction_Vectors.Vector;

      --  For each AST subtree we generate a piece of program.
      --  Some instructions in the piece are left unlinked.
      --  They should be patched to link them to instructions in other pieces.
      --  A piece runs from very first instruction to some unlinked instruction
      --  Relative instruction addresses make program pieces position
      --  independent.

      type Node is record
         Program     : Instruction_Vectors.Vector;
         --  A slice of program. It runs from the first instruction
         Ends        : Address_Vectors.Vector;
         --  List of unlinked instructions to back-patch
      end record;

      To_Be_Patched : constant Instruction_Address := Instruction_Address'Last;
      --  A marker for instruction address to be patched

      First_Instruction : constant Address_Vectors.Vector :=
        Address_Vectors.To_Vector (1, Length => 1);
      --  Singleton pointing to index = 1

      procedure Patch
        (Code    : in out Instruction;
         Address : Instruction_Offset);
      --  Modify Code.Next (or Code.Fallback) to point Address

      function Create_Character
        (Value : VSS.Characters.Virtual_Character) return Node;
      --  Generate <char[next:unlinked]>

      function Create_Any_Character return Node;
      --  Generate <class[0,10FFFF,next:unlinked]> for Dot_Matches_Everything
      --  or [^\n\r\u2028-\2029] otherwise

      function Create_Character_Range
        (From, To : VSS.Characters.Virtual_Character) return Node;
      --  Generate <class[from,to,next:unlinked]>

      function Create_Simple_Assertion
        (Kind : Simple_Assertion_Kind) return Node;
      --  Generate <assertion[kind,next:unlinked]>

      function Create_General_Category_Set
        (Value : Name_Sets.General_Category_Set) return Node;
      --  Generate <category[next:unlinked]>

      function Create_Negated_Class (Left : Node) return Node;
      --  Generate <s:negate[next:unlinked]><left[next:s]>

      function Create_Sequence (Left, Right : Node) return Node;
      --  Generate <left[next:right]><right[next:unlinked]>

      function Create_Alternative (Left, Right : Node) return Node;
      --  Generate <split r>
      --           <left[next:unlinked]>
      --           <r:right[next:unlinked]>

      function Create_Star (Left : Node) return Node;
      --  Generate <s:split[fallback:unlinked]>
      --           <left[next:s]>

      function Create_Plus (Left : Node) return Node;
      --  Generate <l:left[next:s]>
      --           <s:split[next:l,fallback:unlinked]>

      function Create_Group
        (Left : Node;
         From : Positive;
         To   : Natural) return Node;
      --  Generate <save[2*group+1,next:1]>
      --           <left[next:s]>
      --           <s:save[2*group+2,next:unlinked]>

      function Create_Empty return Node;
      --  Generate <nop[next:unlinked]>

      ------------------------
      -- Create_Alternative --
      ------------------------

      function Create_Alternative (Left, Right : Node) return Node is
         use type Address_Vectors.Vector;

         Code : constant Instruction :=
           (Kind      => Split,
            Next      => 1,
            Fallback  => Left.Program.Last_Index + 1);
      begin
         return Result : Node :=
           (Program     => Code & Left.Program & Right.Program,
            Ends        => Left.Ends & Right.Ends)
         do
            --  Shift Left.Ends by 1 (for the split instruction)
            for J in 1 .. Left.Ends.Last_Index loop
               Result.Ends (J) := Result.Ends (J) + 1;
            end loop;

            --  Shift Right.Ends by Left.Program.Length + 1
            for J in Left.Ends.Last_Index + 1 .. Result.Ends.Last_Index loop
               Result.Ends (J) := Result.Ends (J)
                 + Left.Program.Last_Index + 1;
            end loop;
         end return;
      end Create_Alternative;

      --------------------------
      -- Create_Any_Character --
      --------------------------

      function Create_Any_Character return Node is
         subtype Char is VSS.Characters.Virtual_Character;
      begin
         if Options (Dot_Matches_Everything) then
            return Create_Character_Range
              (From => Char (VSS.Unicode.Code_Point_Character'First),
               To   => Char (VSS.Unicode.Code_Point_Character'Last));

         else
            declare
               CR : constant Node :=
                 Create_Character (VSS.Characters.Latin.Carriage_Return);

               LF : constant Node :=
                 Create_Character (VSS.Characters.Latin.Line_Feed);

               LS_PS : constant Node := Create_Character_Range
                 (VSS.Characters.Punctuations.Line_Separator,
                  VSS.Characters.Punctuations.Paragraph_Separator);

            begin
               return
                 Create_Negated_Class
                   (Create_Alternative
                     (CR, Create_Alternative (LF, LS_PS)));
            end;
         end if;
      end Create_Any_Character;

      ----------------------
      -- Create_Character --
      ----------------------

      function Create_Character
        (Value : VSS.Characters.Virtual_Character) return Node
      is
         Code : constant Instruction :=
           (Kind      => Character,
            Next      => To_Be_Patched,
            Character => Value);
      begin
         return
           (Program => Instruction_Vectors.To_Vector (Code, Length => 1),
            Ends    => First_Instruction);
      end Create_Character;

      function Create_Character_Range
        (From, To : VSS.Characters.Virtual_Character) return Node
      is
         Code : constant Instruction :=
           (Kind => Class,
            Next => To_Be_Patched,
            From => From,
            To   => To);
      begin
         return
           (Program => Instruction_Vectors.To_Vector (Code, Length => 1),
            Ends    => First_Instruction);
      end Create_Character_Range;

      ------------------
      -- Create_Empty --
      ------------------

      function Create_Empty return Node is
         Code : constant Instruction :=
           (Kind      => No_Operation,
            Next      => To_Be_Patched);
      begin
         return
           (Program => Instruction_Vectors.To_Vector (Code, Length => 1),
            Ends    => First_Instruction);
      end Create_Empty;

      ---------------------------------
      -- Create_General_Category_Set --
      ---------------------------------

      function Create_General_Category_Set
        (Value : Name_Sets.General_Category_Set) return Node
      is
         Code : constant Instruction :=
           (Kind     => Category,
            Next     => To_Be_Patched,
            Category => Value);
      begin
         return
           (Program => Instruction_Vectors.To_Vector (Code, Length => 1),
            Ends    => First_Instruction);
      end Create_General_Category_Set;

      ------------------
      -- Create_Group --
      ------------------

      function Create_Group
        (Left  : Node;
         From  : Positive;
         To    : Natural) return Node
      is
         Open : constant Instruction :=
           (Kind => Save,
            Tag  => Tag_Number (2 * From + 1),
            Last => Tag_Number (2 * To + 2),  --  Groups in range From .. To
            Next => 1);

         Close : constant Instruction :=
           (Kind => Save,
            Tag  => Tag_Number (2 * From + 2),
            Last => Tag_Number (2 * From + 1),  --  An empty range
            Next => To_Be_Patched);
      begin
         return Result : Node :=
           (Program => Open & Left.Program & Close,
            Ends    => Address_Vectors.Empty_Vector)
         do
            --  Patch left ends to connect them to the Close
            for J of Left.Ends loop
               Patch
                 (Result.Program (J + 1),
                  Result.Program.Last_Index - J - 1);
            end loop;

            Result.Ends.Append (Result.Program.Last_Index);

            Self.Last_Tag := Tag_Number'Max (Self.Last_Tag, Close.Tag);
         end return;
      end Create_Group;

      --------------------------
      -- Create_Negated_Class --
      --------------------------

      function Create_Negated_Class (Left : Node) return Node is
         Code : constant Instruction :=
           (Kind  => Negate_Class,
            Next  => To_Be_Patched);
      begin
         return Result : Node :=
           (Program     => Code & Left.Program,
            Ends        => First_Instruction)
         do
            --  Patch left ends to connect them to the Negate_Class
            for J of Left.Ends loop
               Patch (Result.Program (J + 1), -J);
            end loop;
         end return;
      end Create_Negated_Class;

      -----------------
      -- Create_Plus --
      -----------------

      function Create_Plus (Left : Node) return Node is
         Code : constant Instruction :=
           (Kind      => Split,
            Next      => -Left.Program.Last_Index,
            Fallback  => To_Be_Patched);
      begin
         return Result : Node :=
           (Program     => Left.Program & Code,
            Ends        => Address_Vectors.To_Vector
                             (Left.Program.Last_Index + 1, Length => 1))
         do
            --  Patch left ends to connect them to the Split
            for J of Left.Ends loop
               Patch (Result.Program (J), Left.Program.Last_Index + 1 - J);
            end loop;
         end return;
      end Create_Plus;

      ---------------------
      -- Create_Sequence --
      ---------------------

      function Create_Sequence (Left, Right : Node) return Node is
      begin
         return Result : Node :=
           (Program => Left.Program & Right.Program,
            Ends    => Right.Ends)
         do
            --  Patch left ends to connect them to Right.Program'First
            for J of Left.Ends loop
               Patch (Result.Program (J), Left.Program.Last_Index + 1 - J);
            end loop;

            --  Shift Result.Ends by Left.Program.Length
            for J of Result.Ends loop
               J := J + Left.Program.Last_Index;
            end loop;
         end return;
      end Create_Sequence;

      -----------------------------
      -- Create_Simple_Assertion --
      -----------------------------

      function Create_Simple_Assertion
        (Kind : Simple_Assertion_Kind) return Node
      is
         Code : constant Instruction :=
           (Kind      => Assertion,
            Next      => To_Be_Patched,
            Assertion => Kind);
      begin
         return
           (Program => Instruction_Vectors.To_Vector (Code, Length => 1),
            Ends    => First_Instruction);
      end Create_Simple_Assertion;

      -----------------
      -- Create_Star --
      -----------------

      function Create_Star (Left : Node) return Node is
         Code : constant Instruction :=
           (Kind      => Split,
            Next      => 1,
            Fallback  => To_Be_Patched);
      begin
         return Result : Node :=
           (Program     => Code & Left.Program,
            Ends        => First_Instruction)
         do
            --  Patch left ends to connect them to the Split
            for J of Left.Ends loop
               Patch (Result.Program (J + 1), -J);
            end loop;
         end return;
      end Create_Star;

      -----------
      -- Patch --
      -----------

      procedure Patch
        (Code    : in out Instruction;
         Address : Instruction_Offset) is
      begin
         if Code.Kind = Split and then Code.Fallback = To_Be_Patched then
            Code.Fallback := Address;
         else
            pragma Assert (Code.Next = To_Be_Patched);
            Code.Next := Address;
         end if;
      end Patch;

      package Parser is new VSS.Regular_Expressions.ECMA_Parser (Node);

      --  The final program is
      --  <save tag=1>
      --  <AST.program>
      --  <save tag=2>
      --  <match>

      Save_1 : constant Instruction :=
        (Kind => Save, Next => 1, Tag  => 1, Last => 0);

      Save_2 : constant Instruction :=
        (Kind => Save, Next => 1, Tag  => 2, Last => 0);

      Final : constant Instruction := (Kind => Match, Next => 0);

      Max_Threads : Natural := 1;  --  One for Match instruction
      Error       : VSS.Strings.Virtual_String;
      Root        : Node;
      Cursor      : VSS.Strings.Character_Iterators.Character_Iterator :=
        Pattern.At_First_Character;

   begin
      Self.Last_Tag := 2;

      Parser.Parse_Pattern
        (Cursor => Cursor,
         Error  => Error,
         Result => Root);

      Self.Program := new Instruction_Array (1 .. Root.Program.Last_Index + 3);
      Self.Program (1) := Save_1;

      for J in 1 .. Root.Program.Last_Index loop
         declare
            Code : constant Instruction := Root.Program.Element (J);
         begin
            Self.Program (J + 1) := Code;

            for Code of Root.Program loop
               if Code.Kind in Character .. Negate_Class then
                  --  Increment for each character instruction
                  Max_Threads := Max_Threads + 1;
               end if;
            end loop;
         end;
      end loop;

      Self.Initialize (Options, Error, Pattern);
      Self.Max_Threads := Max_Threads;

      Self.Program (Self.Program'Last - 1) := Save_2;
      Self.Program (Self.Program'Last) :=  Final;

      --  Patch ends to connect them to Save_2
      for J of Root.Ends loop
         Patch (Self.Program (J + 1), Root.Program.Last_Index + 1 - J);
      end loop;

      Fit := True;
   end Parse;

end VSS.Regular_Expressions.Pike_Engines;
