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

with VSS.Regular_Expressions.ECMA_Parser;
with VSS.Strings.Character_Iterators;
with VSS.Implementation.Strings;
with VSS.Unicode;
with VSS.Strings.Cursors.Internals;
with VSS.Regular_Expressions.Matches;
with VSS.Strings.Cursors.Markers.Internals;
with VSS.Strings.Internals;
with VSS.Implementation.String_Handlers;

package body VSS.Regular_Expressions.Pike_Engines is

   -----------
   -- Match --
   -----------

   overriding procedure Match
     (Self    : Engine;
      Subject : VSS.Strings.Virtual_String;
      Options : Match_Options := No_Match_Options;
      Result  : out Match_Access)
   is
      use type VSS.Characters.Virtual_Character;

      type Tag_Array is array (1 .. Self.Last_Tag) of
        VSS.Implementation.Strings.Cursor;
      --  First two items represent whole match boundaries, while others
      --  represent groups voubdaries.
      --  Odd items represent left boundaries, even items - right.
      --  Even items point to next character after the bound.

      Unset_Tags : constant Tag_Array := (1 .. Self.Last_Tag => <>);
      --  All unset tags have invalid values by default

      type Thread_State is record
         PC   : Instruction_Address;
         Tags : Tag_Array;
      end record;

      procedure Step_Backward
        (Text : VSS.Strings.Virtual_String;
         Cursor : in out VSS.Implementation.Strings.Cursor);
      --  Shift Cursor one character backward in string Text

      procedure Append_State
        (Cursor   : VSS.Implementation.Strings.Cursor;
         PC       : Instruction_Address;
         New_Tags : Tag_Array);
      --  This procedure finds all states reachable from the given by
      --  non-character instructions and appends new thread states to the
      --  Next global variable.

      package State_Vectors is new Ada.Containers.Vectors
        (Positive, Thread_State);

      --  Active and Next contains only Match instructions or instructions
      --  accepting a character.

      Active     : State_Vectors.Vector;
      --  Active threads in the current position
      Next       : State_Vectors.Vector;
      --  Threads to run in the next position
      Found      : Boolean := False;
      --  Some match has been found
      Final_Tags : Tag_Array;
      --  If a match has been found, corresponding tag values

      Step  : Natural := 1;
      Steps : array (1 .. Self.Program.Last_Index) of Natural := (others => 0);
      --  A filter to protect the Next from duplicated states

      ------------------
      -- Append_State --
      ------------------

      procedure Append_State
        (Cursor   : VSS.Implementation.Strings.Cursor;
         PC       : Instruction_Address;
         New_Tags : Tag_Array)
      is
         Code : constant Instruction := Self.Program (PC);
      begin
         if Steps (PC) = Step then
            return;
         end if;

         Steps (PC) := Step;

         case Code.Kind is
            when Character | Match =>
               Next.Append ((PC, New_Tags));
            when Split =>
               Append_State (Cursor, PC + Code.Next, New_Tags);
               Append_State (Cursor, PC + Code.Fallback, New_Tags);
            when Save =>
               declare
                  Updated : Tag_Array := New_Tags;
               begin
                  Updated (Code.Tag) := Cursor;
                  Append_State (Cursor, PC + Code.Next, Updated);
               end;
            when No_Operation =>
               Append_State (Cursor, PC + Code.Next, New_Tags);
         end case;
      end Append_State;

      -------------------
      -- Step_Backward --
      -------------------

      procedure Step_Backward
        (Text   : VSS.Strings.Virtual_String;
         Cursor : in out VSS.Implementation.Strings.Cursor)
      is
         use type VSS.Unicode.UTF8_Code_Unit_Offset;
         use type VSS.Unicode.UTF16_Code_Unit_Offset;
         use type VSS.Implementation.Strings.String_Handler_Access;

         Ignore : Boolean;
         Data   : constant VSS.Strings.Internals.String_Data_Constant_Access :=
           VSS.Strings.Internals.Data_Access_Constant (Text);

         Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
           VSS.Implementation.Strings.Handler (Data.all);
      begin
         if VSS.Implementation.Strings.Is_Invalid (Cursor) then
            null;
         elsif Handler = null then
            Cursor := (0, -1, -1);
         else
            Ignore := Handler.Backward (Data.all, Cursor);
         end if;
      end Step_Backward;

      Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
        Subject.First_Character;

      Pos : constant not null
        VSS.Strings.Cursors.Internals.Cursor_Constant_Access :=
          VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant (Cursor);
      --  Access to position of the Cursor
   begin
      Active.Reserve_Capacity (Ada.Containers.Count_Type (Self.Max_Threads));
      Next.Reserve_Capacity (Ada.Containers.Count_Type (Self.Max_Threads));

      --  Start a new thread at the beginning of the sample
      Append_State (Pos.all, 1, Unset_Tags);

      if Cursor.Has_Element then
         loop
            declare
               Char : constant VSS.Characters.Virtual_Character :=
                 Cursor.Element;

               --  Shift Cursor, Pos points to the next character after Char
               Again : constant Boolean := Cursor.Forward;
            begin
               Active.Move (Source => Next);  --  Assign Next to Active
               Step := Step + 1;

               --  Run each active thread
               for X of Active loop
                  declare
                     Code : constant Instruction := Self.Program (X.PC);
                  begin
                     case Code.Kind is
                        when Character =>
                           if Code.Character = Char then
                              Append_State (Pos.all, X.PC + Code.Next, X.Tags);
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

               exit when not Again or (Next.Is_Empty and Found);

               if not Found then
                  --  Start new thread from the current character if we haven't
                  --  found any match yet.
                  Append_State (Pos.all, 1, Unset_Tags);
               end if;
            end;
         end loop;
      end if;

      Active.Move (Source => Next);

      --  Find a reached final state
      for X of Active loop
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

      if Found then
         Result := new VSS.Regular_Expressions.Matches.Match
           (Length => Natural (Self.Last_Tag / 2));

         declare
            Index : Tag_Number := Final_Tags'First;
         begin
            Result.Subject := Subject;

            for J in Result.Markers'Range loop
               declare
                  From : VSS.Implementation.Strings.Cursor renames
                    Final_Tags (Index);

                  To   : VSS.Implementation.Strings.Cursor renames
                    Final_Tags (Index + 1);
               begin
                  Step_Backward (Result.Subject, To);

                  Result.Markers (J) :=
                    VSS.Strings.Cursors.Markers.Internals.New_Segment_Marker
                      (Result.Subject,
                       First => From,
                       Last  => To);

                  Index := Index + 2;
               end;
            end loop;
         end;
      else
         Result := new VSS.Regular_Expressions.Matches.Match (Length => 0);
      end if;

      Result.Has_Match := Found;
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
      use type Instruction_Vectors.Vector;

      package Address_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Instruction_Address);
      --  List of instruction addresses

      --  For each AST subtree we generate a piece of program.
      --  Some instructions in the piece are left unlinked.
      --  They should be patched to link them to instructions in other pieces.
      --  A piece runs from very first instruction to some unliked instruction.
      --  Relative instruction addresses make program pieces position
      --  independent.

      type Node is record
         Program     : Instruction_Vectors.Vector;
         --  A slice of program. It runs from the first instruction
         Ends        : Address_Vectors.Vector;
         --  List of unliked instructions to back-patch
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
      --  Generate <char[next:unliked]>

      function Create_Sequence (Left, Right : Node) return Node;
      --  Generate <left[next:right]><right[next:unliked]>

      function Create_Alternative (Left, Right : Node) return Node;
      --  Generate <split r>
      --           <left[next:unliked]>
      --           <r:right[next:unliked]>

      function Create_Star (Left : Node) return Node;
      --  Generate <s:split[fallback:unliked]>
      --           <left[next:s]>

      function Create_Empty return Node;
      --  Generate <nop[next:unliked]>

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
      -- <save tag=1>
      -- <AST.program>
      -- <save tag=2>
      -- <match>

      Save_1 : constant Instruction :=
        (Kind => Save,
         Next => 1,
         Tag  => 1);

      Save_2 : constant Instruction :=
        (Kind => Save,
         Next => 1,
         Tag  => 2);

      Final : constant Instruction := (Kind => Match, Next => 0);

      Max_Threads : Natural := 1;  --  threads required to execute
      Error       : VSS.Strings.Virtual_String;
      Root        : Node;
      Cursor      : VSS.Strings.Character_Iterators.Character_Iterator :=
        Pattern.First_Character;
   begin
      Parser.Parse_Pattern
        (Cursor => Cursor,
         Error  => Error,
         Result => Root);

      for Code of Root.Program loop
         if Code.Kind = Character then
            Max_Threads := Max_Threads + 1;
         end if;
      end loop;

      Self.Initialize (Options, Error, Pattern);
      Self.Max_Threads := Max_Threads;
      Self.Last_Tag := 2;
      Self.Program := Save_1 & Root.Program & Save_2 & Final;

      --  Patch ends to connect them to Save_2
      for J of Root.Ends loop
         Patch (Self.Program (J + 1), Root.Program.Last_Index + 1 - J);
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

end VSS.Regular_Expressions.Pike_Engines;
