with Magic_Strings.Conversions;

procedure Test_Character_Iterators is
   S : Magic_Strings.Magic_String :=
     Magic_Strings.Conversions.To_Magic_String ("ASCII Кириллица");
   J : Magic_Strings.Character_Iterator := S.First_Character;

begin
   null;
end Test_Character_Iterators;
