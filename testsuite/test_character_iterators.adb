with Magic.Strings.Conversions;
with Magic.Strings.Iterators.Characters;
with Magic.Unicode;

procedure Test_Character_Iterators is

   use type Magic.Strings.Character_Count;
   use type Magic.Unicode.UTF8_Code_Unit_Count;
   use type Magic.Unicode.UTF16_Code_Unit_Count;

   type Position_Data is record
      UTF8_Offset  : Magic.Unicode.UTF8_Code_Unit_Count;
      UTF16_Offset : Magic.Unicode.UTF16_Code_Unit_Count;
   end record;

   S : constant Magic.Strings.Magic_String :=
     Magic.Strings.Conversions.To_Magic_String ("ASCII Кириллица");

   D : constant array (Magic.Strings.Character_Index range <>) of Position_Data :=
    ((0, 0),
     (1, 1),
     (2, 2),
     (3, 3),
     (4, 4),
     (5, 5),
     (6, 6),
     (8, 7),
     (10, 8),
     (12, 9),
     (14, 10),
     (16, 11),
     (18, 12),
     (20, 13),
     (22, 14));

   J : Magic.Strings.Iterators.Characters.Character_Iterator :=
     S.First_Character;
   C : Magic.Strings.Character_Index := 1;

begin
   loop
      if C /= J.Character_Index then
         raise Program_Error;
      end if;

      if J.Character_Index not in D'Range then
         raise Program_Error;
      end if;

      if J.UTF8_Offset /= D (C).UTF8_Offset then
         raise Program_Error;
      end if;

      if J.UTF16_Offset /= D (C).UTF16_Offset then
         raise Program_Error;
      end if;

      C := C + 1;

      if not J.Forward then
         if C <= D'Last then
            raise Program_Error;
         end if;

         exit;
      end if;
   end loop;
end Test_Character_Iterators;
