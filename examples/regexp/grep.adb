with Ada.Wide_Wide_Text_IO;

with VSS.Application;
with VSS.Regular_Expressions;
with VSS.Strings;
with VSS.Strings.Conversions;

procedure Grep is

   procedure Print_Match
     (Match : VSS.Regular_Expressions.Regular_Expression_Match;
      Group : Natural := 0);
   --  Print Match information

   procedure Print_Match
     (Match : VSS.Regular_Expressions.Regular_Expression_Match;
      Group : Natural := 0) is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (":"
         & Match.First_Marker (Group).Character_Index'Wide_Wide_Image
         & " .."
         & Match.Last_Marker (Group).Character_Index'Wide_Wide_Image
         & " => '"
         & VSS.Strings.Conversions.To_Wide_Wide_String
           (Match.Captured (Group))
         & "'");
   end Print_Match;

   Pattern : constant VSS.Strings.Virtual_String :=
     VSS.Application.Arguments.Element (1);

   Reg_Exp : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression (Pattern);
begin
   if Reg_Exp.Is_Valid then
      while not Ada.Wide_Wide_Text_IO.End_Of_File loop
         declare
            Line : constant Wide_Wide_String := Ada.Wide_Wide_Text_IO.Get_Line;
            Text : constant VSS.Strings.Virtual_String :=
              VSS.Strings.To_Virtual_String (Line);
            Match : constant VSS.Regular_Expressions.Regular_Expression_Match
              := Reg_Exp.Match (Text);
         begin
            if Match.Has_Match then
               Ada.Wide_Wide_Text_IO.Put ("Match found");
               Print_Match (Match);

               for J in 1 .. Reg_Exp.Capture_Group_Count loop
                  if Match.Has_Capture (J) then
                     Ada.Wide_Wide_Text_IO.Put ("   ");
                     Ada.Wide_Wide_Text_IO.Put (J'Wide_Wide_Image);
                     Print_Match (Match, J);
                  end if;
               end loop;
            end if;
         end;
      end loop;
   else
      Ada.Wide_Wide_Text_IO.Put ("Invalid regexp:");
      Ada.Wide_Wide_Text_IO.Put
        (VSS.Strings.Conversions.To_Wide_Wide_String (Reg_Exp.Error_String));
      Ada.Wide_Wide_Text_IO.New_Line;
   end if;
end Grep;
