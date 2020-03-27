with Magic.Characters;
with Magic.Strings.Conversions;
with Magic.Strings.Iterators.Characters;
with Magic.Unicode;

procedure Test_Character_Iterators is

   use type Magic.Characters.Magic_Character;
   use type Magic.Strings.Character_Count;
   use type Magic.Unicode.UTF8_Code_Unit_Count;
   use type Magic.Unicode.UTF16_Code_Unit_Count;

   type Position_Data is record
      Character    : Magic.Characters.Magic_Character;
      UTF8_Offset  : Magic.Unicode.UTF8_Code_Unit_Count;
      UTF16_Offset : Magic.Unicode.UTF16_Code_Unit_Count;
   end record;

   --  "ASCII Кириллица ⊗∬ 𝛻𝜕 "
   S : constant Magic.Strings.Magic_String :=
     Magic.Strings.Conversions.To_Magic_String
       ((Character'Val(16#41#),
        Character'Val(16#53#),
        Character'Val(16#43#),
        Character'Val(16#49#),
        Character'Val(16#49#),
        Character'Val(16#20#),
        Character'Val(16#D0#),
        Character'Val(16#9A#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#80#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#BB#),
        Character'Val(16#D0#),
        Character'Val(16#B8#),
        Character'Val(16#D1#),
        Character'Val(16#86#),
        Character'Val(16#D0#),
        Character'Val(16#B0#),
        Character'Val(16#20#),
        Character'Val(16#E2#),
        Character'Val(16#8A#),
        Character'Val(16#97#),
        Character'Val(16#E2#),
        Character'Val(16#88#),
        Character'Val(16#AC#),
        Character'Val(16#20#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9B#),
        Character'Val(16#BB#),
        Character'Val(16#F0#),
        Character'Val(16#9D#),
        Character'Val(16#9C#),
        Character'Val(16#95#),
        Character'Val(16#20#)));

   D : constant array (Magic.Strings.Character_Index range <>) of Position_Data :=
    (('A', 0, 0),     --  'A' 1
     ('S', 1, 1),     --  'S' 2
     ('C', 2, 2),     --  'C' 3
     ('I', 3, 3),     --  'I' 4
     ('I', 4, 4),     --  'I' 5
     (' ', 5, 5),     --  ' ' 6
     ('К', 6, 6),     --  'К' 7
     ('и', 8, 7),     --  'и' 8
     ('р', 10, 8),    --  'р' 9
     ('и', 12, 9),    --  'и' 10
     ('л', 14, 10),   --  'л' 11
     ('л', 16, 11),   --  'л' 12
     ('и', 18, 12),   --  'и' 13
     ('ц', 20, 13),   --  'ц' 14
     ('а', 22, 14),   --  'а' 15
     (' ', 24, 15),   --  ' ' 16
     ('⊗', 25, 16),   --  '⊗' 17
     ('∬', 28, 17),   --  '∬' 18
     (' ', 31, 18),   --  ' ' 19
     ('𝛻', 32, 19),   --  '𝛻' 17
     ('𝜕', 36, 21),   --  '𝜕' 18
     (' ', 40, 23));  --  ' ' 19

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

      if J.Element /= D (C).Character then
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
