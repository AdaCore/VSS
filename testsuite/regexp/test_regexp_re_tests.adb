------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Wide_Wide_Text_IO;

with VSS.Characters;
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
      Check   : out Check_Value);

   procedure Verify
     (Line   : Wide_Wide_String;
      Match  : VSS.Regular_Expressions.Regular_Expression_Match;
      Expr   : VSS.Strings.Virtual_String;
      Expect : VSS.Strings.Virtual_String;
      Total  : in out Natural);

   function Image (Value : VSS.Strings.Character_Count)
     return VSS.Strings.Virtual_String;

   function Expand_Sample (Value : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String;
   --  Interprete `\x{AA}`

   Space : constant Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set :=
     Ada.Strings.Wide_Wide_Maps.To_Set
       (Ada.Characters.Wide_Wide_Latin_1.HT);

   -------------------
   -- Expand_Sample --
   -------------------

   function Expand_Sample (Value : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String
   is
      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        Value.Split ('\');

      Result : VSS.Strings.Virtual_String := List (1);
   begin

      for J in 2 .. List.Length loop
         if List (J).Starts_With ("x{") then
            declare
               use type VSS.Characters.Virtual_Character;

               Item  : constant VSS.Strings.Virtual_String := List (J);
               Value : VSS.Strings.Virtual_String;
               X     : VSS.Strings.Character_Iterators.Character_Iterator :=
                 Item.At_First_Character;
            begin
               if X.Forward then --  Skip x
                  while X.Forward and then X.Element /= '}' loop
                     Value.Append (X.Element);
                  end loop;

                  Result.Append
                    (VSS.Characters.Virtual_Character'Val
                      (Positive'Value
                        ("16#" &
                         VSS.Strings.Conversions.To_UTF_8_String (Value) &
                         "#")));

                  if X.Forward then --  Skip }
                     Result.Append (Item.Slice (X, Item.At_Last_Character));
                  end if;
               end if;
            end;
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
        VSS.Strings.Character_Count'Wide_Wide_Image (Value);
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
      Check   : out Check_Value)
   is
      From : array (1 .. 5) of Positive;
      To   : array (0 .. 5) of Natural := (others => 0);
   begin
      --  Parse 5 columns
      --  1 => pattern
      --  2 => sample
      --  3 => has match
      --  4 => expression to evaluate
      --  5 => evaluated value
      for J in From'Range loop
         Ada.Strings.Wide_Wide_Fixed.Find_Token
           (Source => Line,
            Set    => Space,
            From   => To (J - 1) + 1,
            Test   => Ada.Strings.Outside,
            First  => From (J),
            Last   => To (J));
      end loop;

      if Line (From (1)) /= '/' then
         Pattern := VSS.Strings.To_Virtual_String (Line (From (1) .. To (1)));
      elsif Line (To (1)) = '/' then  --  Pattern is `/regexp/`
         Pattern := VSS.Strings.To_Virtual_String
              (Line (From (1) + 1 .. To (1) - 1));
      else  --  Pattern in form of `/regexp/FLAGS` isn't supported yet
         Check := (Kind => Skip);
         return;
      end if;

      Sample := VSS.Strings.To_Virtual_String (Line (From (2) .. To (2)));
      Sample := Expand_Sample (Sample);

      declare
         Status : constant Wide_Wide_String := Line (From (3) .. To (3));
         Expr   : constant Wide_Wide_String := Line (From (4) .. To (4));
         Expect : constant Wide_Wide_String := Line (From (5) .. To (5));
      begin
         case Status (Status'First) is
            when 'y' =>
               Check := (Match,
                         VSS.Strings.To_Virtual_String (Expr),
                         Expand_Sample
                           (VSS.Strings.To_Virtual_String (Expect)));
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
                        Result.Append (Image (Marker.Character_Index - 1));
                     end;

                     pragma Assert (Cursor.Element = ']');

                  when '+' =>  --  &+[X] - the X group end offset
                     pragma Assert (Cursor.Forward);
                     pragma Assert (Cursor.Element = '[');
                     pragma Assert (Cursor.Forward);
                     pragma Assert (Cursor.Element in '0' .. '9');

                     declare
                        Index : constant Natural := Read_Natural (Cursor);

                        Marker : constant
                          VSS.Strings.Cursors.Markers.Character_Marker :=
                            Match.Last_Marker (Index);
                     begin
                        Result.Append (Image (Marker.Character_Index));
                     end;

                     pragma Assert (Cursor.Element = ']');

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

   Total : Natural := 0;

   Last_Pattern : VSS.Regular_Expressions.Regular_Expression;
   Last_Sample  : VSS.Strings.Virtual_String;
   Last_Match   : VSS.Regular_Expressions.Regular_Expression_Match;

begin
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

               Pattern : VSS.Strings.Virtual_String;
               Sample  : VSS.Strings.Virtual_String;
               Check   : Check_Value;
            begin
               Parse_Line (Line, Pattern, Sample, Check);

               if Check.Kind /= Skip then
                  if Last_Pattern.Pattern /= Pattern then
                     Last_Pattern :=
                       VSS.Regular_Expressions.To_Regular_Expression (Pattern);

                     Last_Sample := VSS.Strings.Empty_Virtual_String;
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
                              Last_Match,
                              Check.Expr,
                              Check.Expect,
                              Total);
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
end Test_RegExp_RE_Tests;
