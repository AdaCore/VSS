with Magic.Strings.Conversions;

procedure Test_Character_Iterators is
   S : Magic.Strings.Magic_String :=
     Magic.Strings.Conversions.To_Magic_String ("ASCII Кириллица");
   J : Magic.Strings.Character_Iterator := S.First_Character;

begin
   null;
end Test_Character_Iterators;
