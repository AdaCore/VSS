--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with VSS.Application;
with VSS.Characters;
with VSS.Characters.Latin;
with VSS.Regular_Expressions;
with VSS.Strings;
with VSS.Strings.Cursors.Markers;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.String_Vectors;

procedure Test_RegExp_RE_Tests is
   pragma Assertion_Policy (Check);

   type Check_Kind is (Skip, Match, Dont_Match);

   type Check_Value (Kind : Check_Kind := Skip) is record
      case Kind is
         when Match =>
            Expr   : VSS.Strings.Virtual_String;
            Expect : VSS.Strings.Virtual_String;
         when others =>
            null;
      end case;
   end record;

   procedure Parse_Line
     (Line    : Wide_Wide_String;
      Pattern : out VSS.Strings.Virtual_String;
      Sample  : out VSS.Strings.Virtual_String;
      Check   : out Check_Value;
      Options : out VSS.Regular_Expressions.Pattern_Options);

   procedure Verify
     (Line   : Wide_Wide_String;
      Regexp : VSS.Regular_Expressions.Regular_Expression;
      Match  : VSS.Regular_Expressions.Regular_Expression_Match;
      Expr   : VSS.Strings.Virtual_String;
      Expect : VSS.Strings.Virtual_String;
      Total  : in out Natural);

   function Image (Value : VSS.Strings.Character_Count)
     return VSS.Strings.Virtual_String;

   function Expand_Sample (Value : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String;
   --  Interprete `\n`, `\x{AA}`

   Tab : constant VSS.Characters.Virtual_Character :=
     VSS.Characters.Virtual_Character
       (Ada.Characters.Wide_Wide_Latin_1.HT);

   -------------------
   -- Expand_Sample --
   -------------------

   function Expand_Sample (Value : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String
   is
      procedure Argument
        (Item  : VSS.Strings.Virtual_String;
         Value : out VSS.Strings.Virtual_String;
         Rest  : out VSS.Strings.Virtual_String);
      --  If Item containis `?{aaa}bbb` return Value=aaa and Rest=bbb.

      function Char (Pos : Positive) return VSS.Characters.Virtual_Character is
        (VSS.Characters.Virtual_Character'Val (Pos));

      --------------
      -- Argument --
      --------------

      procedure Argument
        (Item  : VSS.Strings.Virtual_String;
         Value : out VSS.Strings.Virtual_String;
         Rest  : out VSS.Strings.Virtual_String)
      is
         use type VSS.Characters.Virtual_Character;

         X : VSS.Strings.Character_Iterators.Character_Iterator :=
           Item.At_First_Character;
      begin
         if X.Forward then --  Skip x
            while X.Forward and then X.Element /= '}' loop
               Value.Append (X.Element);
            end loop;

            if X.Forward then --  Skip }
               Rest.Append (Item.Slice (X, Item.At_Last_Character));
            end if;
         end if;
      end Argument;

      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        Value.Split ('\');

      Result : VSS.Strings.Virtual_String := List (1);
   begin

      for J in 2 .. List.Length loop
         if List (J).Starts_With ("x{") then
            declare
               Value : VSS.Strings.Virtual_String;
            begin
               Argument (List (J), Value, Result);

               Result.Prepend
                 (Char
                   (Positive'Value
                     ("16#" &
                      VSS.Strings.Conversions.To_UTF_8_String (Value) &
                      "#")));
            end;
         elsif List (J).Starts_With ("N{") then
            declare
               use type VSS.Strings.Virtual_String;
               Value : VSS.Strings.Virtual_String;
            begin
               Argument (List (J), Value, Result);

               if Value = "KELVIN SIGN" then
                  Result.Prepend (Char (16#212A#));
               elsif Value = "LATIN SMALL LIGATURE ST" then
                  Result.Prepend (Char (16#FB06#));
               elsif Value = "LATIN SMALL LIGATURE LONG S T" then
                  Result.Prepend (Char (16#FB05#));
               elsif Value = "LATIN SMALL LETTER LONG S" then
                  Result.Prepend (Char (16#017F#));
               elsif Value = "SPACE" then
                  Result.Prepend (Char (16#20#));
               else
                  Result.Prepend ("\N{" & Value & "}");
               end if;
            end;
         elsif List (J).Starts_With ("n") then
            Result.Append (VSS.Characters.Latin.Line_Feed);
         else
            Result.Append ('\');
            Result.Append (List (J));
         end if;
      end loop;

      return Result;
   end Expand_Sample;

   -----------
   -- Image --
   -----------

   function Image (Value : VSS.Strings.Character_Count)
     return VSS.Strings.Virtual_String
   is
      Img : constant Wide_Wide_String :=
        VSS.Strings.Character_Count'Base'Wide_Wide_Image (Value);
   begin
      return VSS.Strings.To_Virtual_String (Img (2 .. Img'Last));
   end Image;

   ----------------
   -- Parse_Line --
   ----------------

   procedure Parse_Line
     (Line    : Wide_Wide_String;
      Pattern : out VSS.Strings.Virtual_String;
      Sample  : out VSS.Strings.Virtual_String;
      Check   : out Check_Value;
      Options : out VSS.Regular_Expressions.Pattern_Options)
   is
      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        VSS.Strings.To_Virtual_String (Line).Split (Tab);
   begin
      Options := VSS.Regular_Expressions.No_Pattern_Options;
      --  Parse 5 columns
      --  1 => pattern
      --  2 => sample
      --  3 => has match
      --  4 => expression to evaluate
      --  5 => evaluated value

      if not List (1).Starts_With ("/") then
         Pattern := List (1);
      else  --  Pattern in form of `/regexp/FLAGS`
         Pattern := List (1);

         declare
            use type VSS.Characters.Virtual_Character;
            use all type VSS.Regular_Expressions.Pattern_Option;

            From : VSS.Strings.Character_Iterators.Character_Iterator :=
              Pattern.At_First_Character;
            To : VSS.Strings.Character_Iterators.Character_Iterator :=
              Pattern.After_Last_Character;
         begin
            while To.Backward and then To.Element /= '/' loop
               case To.Element is
                  when 's' =>
                     Options (Dot_Matches_Everything) := True;
                  when 'i' =>
                     Options (Case_Insensitive) := True;
                  when 'm' =>
                     Options (Multiline) := True;
                  when 'x' =>
                     Options (Extended_Pattern_Syntax) := True;
                  when 'U' =>
                     Options (Inverted_Greediness) := True;
                  when 'n' =>
                     Options (Dont_Capture) := True;
                  when others =>
                     --  Skipping on an unsupported flag
                     Check := (Kind => Skip);
                     return;
               end case;
            end loop;

            pragma Assert (From.Forward);
            pragma Assert (To.Backward);

            Pattern := Pattern.Slice (From, To);
         end;
      end if;

      Sample := Expand_Sample (List (2));

      declare
         Status : constant VSS.Strings.Virtual_String := List (3);
         Expr   : constant VSS.Strings.Virtual_String := List (4);
         Expect : constant VSS.Strings.Virtual_String := List (5);
      begin
         case Status.At_First_Character.Element is
            when 'y' =>
               Check := (Match, Expr, Expand_Sample (Expect));
            when 'n' =>
               Check := (Kind => Dont_Match);
            when others =>   --  'c'  --  compile error
               Check := (Kind => Skip);
         end case;
      end;
   end Parse_Line;

   ------------
   -- Verify --
   ------------

   procedure Verify
     (Line   : Wide_Wide_String;
      Regexp : VSS.Regular_Expressions.Regular_Expression;
      Match  : VSS.Regular_Expressions.Regular_Expression_Match;
      Expr   : VSS.Strings.Virtual_String;
      Expect : VSS.Strings.Virtual_String;
      Total  : in out Natural)
   is
      use type VSS.Strings.Character_Count;
      use type VSS.Strings.Virtual_String;
      use type VSS.Characters.Virtual_Character;

      function Read_Natural
        (Cursor : in out VSS.Strings.Character_Iterators.Character_Iterator)
         return Natural;
      --  Read natural number from given cursor. Move the cursor to next char.

      function Read_Natural
        (Cursor : in out VSS.Strings.Character_Iterators.Character_Iterator)
         return Natural
      is
         To   : VSS.Strings.Cursors.Markers.Character_Marker;
         From : constant VSS.Strings.Cursors.Markers.Character_Marker :=
           Cursor.Marker;
      begin
         while Cursor.Element in '0' .. '9' loop
            To := Cursor.Marker;
            exit when not Cursor.Forward;
         end loop;

         return Natural'Wide_Wide_Value
           (VSS.Strings.Conversions.To_Wide_Wide_String
              (Expr.Slice (From, To)));
      end Read_Natural;

      Result : VSS.Strings.Virtual_String;
      Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
        Expr.At_First_Character;
      Mark   : VSS.Strings.Cursors.Markers.Character_Marker;
   begin
      loop
         case Cursor.Element is
            when '$' =>
               pragma Assert (Cursor.Forward);

               case Cursor.Element is
                  when '&' =>  --  $& - whole match
                     Result.Append (Match.Captured);

                  when '1' .. '9' =>  --  $X - the X group
                     declare
                        Index : constant Positive := Read_Natural (Cursor);
                     begin
                        Result.Append (Match.Captured (Index));
                        --  step back to last digit:
                        pragma Assert (Cursor.Backward);
                     end;

                  when '-' =>  --  &-[X] - the X group start offset
                     pragma Assert (Cursor.Forward);
                     pragma Assert (Cursor.Element = '[');
                     pragma Assert (Cursor.Forward);
                     pragma Assert (Cursor.Element in '0' .. '9');

                     declare
                        Index : constant Natural := Read_Natural (Cursor);

                        Marker : constant
                          VSS.Strings.Cursors.Markers.Character_Marker :=
                            Match.First_Marker (Index);
                     begin
                        if Marker.Is_Valid then
                           Result.Append (Image (Marker.Character_Index - 1));
                        end if;
                     end;

                     pragma Assert (Cursor.Element = ']');

                  when '+' =>
                     Mark := Cursor.Marker;

                     if Cursor.Forward and then Cursor.Element = '[' then
                        --  &+[X] - the X group end offset
                        pragma Assert (Cursor.Forward);
                        pragma Assert (Cursor.Element in '0' .. '9');

                        declare
                           Index : constant Natural := Read_Natural (Cursor);

                           Marker : constant
                             VSS.Strings.Cursors.Markers.Character_Marker :=
                               Match.Last_Marker (Index);
                        begin
                           if Marker.Is_Valid then
                              Result.Append (Image (Marker.Character_Index));
                           end if;
                        end;

                        pragma Assert (Cursor.Element = ']');
                     else
                        --  &+ - Take rightmost matched group, this isn't what
                        --  Perl does, but should work for tests, I hope

                        for J in reverse 1 .. Regexp.Capture_Group_Count loop
                           if Match.Has_Capture (J) then
                              Result.Append (Match.Captured (J));
                              exit;
                           end if;
                        end loop;

                        Cursor.Set_At (Mark);

                     end if;

                  when '^' =>
                     --  $^N - Take rightmost matched group, this isn't what
                     --  Perl does, but should work for tests, I hope
                     pragma Assert (Cursor.Forward);
                     pragma Assert (Cursor.Element = 'N');

                     for J in reverse 1 .. Regexp.Capture_Group_Count loop
                        if Match.Has_Capture (J) then
                           Result.Append (Match.Captured (J));
                           exit;
                        end if;
                     end loop;

                  when others =>
                     raise Program_Error;
               end case;
            when others =>
               Result.Append (Cursor.Element);
         end case;

         exit when not Cursor.Forward;
      end loop;

      if Result = Expect then
         Total := Total + 1;  --  PASS
      else
         Ada.Wide_Wide_Text_IO.Put ("DIFF: ");
         Ada.Wide_Wide_Text_IO.Put (Line);
         Ada.Wide_Wide_Text_IO.Put (" /= <");
         Ada.Wide_Wide_Text_IO.Put
           (VSS.Strings.Conversions.To_Wide_Wide_String (Result));
         Ada.Wide_Wide_Text_IO.Put_Line (">");
      end if;
   end Verify;

   Total  : Natural := 0;
   Expect : Natural;  --  Expectation for final Total value

   Last_Pattern : VSS.Regular_Expressions.Regular_Expression;
   Last_Sample  : VSS.Strings.Virtual_String;
   Last_Match   : VSS.Regular_Expressions.Regular_Expression_Match;
   Last_Options : VSS.Regular_Expressions.Pattern_Options :=
     VSS.Regular_Expressions.No_Pattern_Options;

begin
   if VSS.Application.Arguments.Is_Empty then
      Expect := 0;
   elsif VSS.Application.Arguments.Last_Element.Starts_With ("-") then
      Ada.Wide_Wide_Text_IO.Put_Line ("Usage: test_regexp_re_tests [count]");
      Ada.Wide_Wide_Text_IO.New_Line;
      Ada.Wide_Wide_Text_IO.Put_Line
        ("where 'count' a number of requred tests to pass.");
      return;
   else
      Expect := Natural'Wide_Wide_Value
        (VSS.Strings.Conversions.To_Wide_Wide_String
          (VSS.Application.Arguments.Last_Element));
   end if;

   while not Ada.Wide_Wide_Text_IO.End_Of_File loop
      declare
         Line : constant Wide_Wide_String := Ada.Wide_Wide_Text_IO.Get_Line;
      begin
         if Line in "" | "__END__"
           or else Line (1) = '#'
         then
            --  skip comments and special lines
            null;
         elsif Line (1) not in ''' then
            declare
               use type VSS.Strings.Virtual_String;
               use type VSS.Regular_Expressions.Pattern_Options;

               Pattern : VSS.Strings.Virtual_String;
               Sample  : VSS.Strings.Virtual_String;
               Check   : Check_Value;
               Options : VSS.Regular_Expressions.Pattern_Options;
            begin
               Parse_Line (Line, Pattern, Sample, Check, Options);

               if Check.Kind /= Skip then
                  if Last_Pattern.Pattern /= Pattern
                    or Options /= Last_Options
                  then
                     Last_Pattern :=
                       VSS.Regular_Expressions.To_Regular_Expression
                         (Pattern, Options);

                     Last_Options := Options;
                     Last_Sample := "not-a-sample";
                  end if;

                  if Last_Sample /= Sample then
                     Last_Sample := Sample;
                     Last_Match := Last_Pattern.Match (Last_Sample);
                  end if;

                  case Check.Kind is
                     when Match =>
                        if Last_Match.Has_Match then
                           Verify
                             (Line,
                              Last_Pattern,
                              Last_Match,
                              Check.Expr,
                              Check.Expect,
                              Total);
                        elsif not Last_Pattern.Error_String.Is_Empty then
                           Ada.Wide_Wide_Text_IO.Put ("ERROR: ");
                           Ada.Wide_Wide_Text_IO.Put (Line);
                           Ada.Wide_Wide_Text_IO.Put (" -> ");
                           Ada.Wide_Wide_Text_IO.Put_Line
                             (VSS.Strings.Conversions.To_Wide_Wide_String
                                (Last_Pattern.Error_String));
                        else
                           Ada.Wide_Wide_Text_IO.Put ("DONT: ");
                           Ada.Wide_Wide_Text_IO.Put_Line (Line);
                        end if;
                     when Dont_Match =>
                        if Last_Match.Has_Match then
                           Ada.Wide_Wide_Text_IO.Put ("MISS: ");
                           Ada.Wide_Wide_Text_IO.Put_Line (Line);
                        else
                           Total := Total + 1;  --  PASS
                        end if;
                     when Skip =>
                        null;
                  end case;
               else
                  Ada.Wide_Wide_Text_IO.Put ("SKIP: ");
                  Ada.Wide_Wide_Text_IO.Put_Line (Line);
               end if;
            exception
               when others =>
                  Ada.Wide_Wide_Text_IO.Put ("CRASH: ");
                  Ada.Wide_Wide_Text_IO.Put_Line (Line);
            end;
         end if;
      end;
   end loop;

   Ada.Wide_Wide_Text_IO.Put ("PASSED:");
   Ada.Wide_Wide_Text_IO.Put_Line (Total'Wide_Wide_Image);

   if Expect > Total then
      Ada.Wide_Wide_Text_IO.Put ("Execution failed! Not enough PASSED tests!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_RegExp_RE_Tests;
