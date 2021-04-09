with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Wide_Wide_Text_IO;

with VSS.Strings;
with VSS.Regular_Expressions;

procedure Test_RegExp_RE_Tests is

   type Check_Kind is (Skip, Match, Dont_Match);

   procedure Parse_Line
     (Line    : Wide_Wide_String;
      Pattern : out VSS.Strings.Virtual_String;
      Sample  : out VSS.Strings.Virtual_String;
      Check   : out Check_Kind;
      Expect  : out VSS.Strings.Virtual_String);

   Space : constant Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set :=
     Ada.Strings.Wide_Wide_Maps.To_Set
       (Ada.Characters.Wide_Wide_Latin_1.HT);

   procedure Parse_Line
     (Line    : Wide_Wide_String;
      Pattern : out VSS.Strings.Virtual_String;
      Sample  : out VSS.Strings.Virtual_String;
      Check   : out Check_Kind;
      Expect  : out VSS.Strings.Virtual_String)
   is
      From : array (1 .. 5) of Positive;
      To   : array (0 .. 5) of Natural := (others => 0);
   begin
      --  Parse 5 columns
      for J in From'Range loop
         Ada.Strings.Wide_Wide_Fixed.Find_Token
           (Source => Line,
            Set    => Space,
            From   => To (J - 1) + 1,
            Test   => Ada.Strings.Outside,
            First  => From (J),
            Last   => To (J));
      end loop;

      Pattern := VSS.Strings.To_Virtual_String (Line (From (1) .. To (1)));
      Sample := VSS.Strings.To_Virtual_String (Line (From (2) .. To (2)));
      Expect := VSS.Strings.To_Virtual_String (Line (From (5) .. To (5)));

      declare
         Status : constant Wide_Wide_String := Line (From (3) .. To (3));
--         Expr   : constant Wide_Wide_String := Line (From (4) .. To (4));
      begin
         Check := (if Status = "y" then Match
                   elsif Status = "n" then Dont_Match
                   else Skip);
      end;
   end Parse_Line;

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
         elsif Line (1) not in ''' | '/' then
            declare
               use type VSS.Strings.Virtual_String;

               Pattern : VSS.Strings.Virtual_String;
               Sample  : VSS.Strings.Virtual_String;
               Check   : Check_Kind;
               Expect  : VSS.Strings.Virtual_String;
            begin
               Parse_Line (Line, Pattern, Sample, Check, Expect);

               if Check /= Skip then
                  if Last_Pattern.Pattern /= Pattern then
                     Last_Pattern :=
                       VSS.Regular_Expressions.To_Regular_Expression (Pattern);

                     Last_Sample := VSS.Strings.Empty_Virtual_String;
                  end if;

                  if Last_Sample /= Sample then
                     Last_Match := Last_Pattern.Match (Sample);
                  end if;

                  case Check is
                     when Match =>
                        if not Last_Match.Has_Match then
                           Ada.Wide_Wide_Text_IO.Put ("DONT: ");
                        elsif Last_Match.Captured = Expect then
                           Ada.Wide_Wide_Text_IO.Put ("PASS: ");
                        else
                           Ada.Wide_Wide_Text_IO.Put ("DIFF: ");
                        end if;
                     when Dont_Match =>
                        if Last_Match.Has_Match then
                           Ada.Wide_Wide_Text_IO.Put ("MISS: ");
                        else
                           Ada.Wide_Wide_Text_IO.Put ("PASS: ");
                        end if;
                     when Skip =>
                        null;
                  end case;

                  Total := Total + 1;
               else
                  Ada.Wide_Wide_Text_IO.Put ("SKIP: ");
               end if;

               Ada.Wide_Wide_Text_IO.Put_Line (Line);

            exception
               when others =>
                  Ada.Wide_Wide_Text_IO.Put ("CRASH: ");
                  Ada.Wide_Wide_Text_IO.Put_Line (Line);
            end;
         end if;
      end;
   end loop;

   Ada.Wide_Wide_Text_IO.Put_Line (Total'Wide_Wide_Image);
end Test_RegExp_RE_Tests;
