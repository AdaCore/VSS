------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with VSS.Strings.Character_Iterators;

with Test_Support;

procedure Test_String is

   procedure Test_Tail;

   ---------------
   -- Test_Tail --
   ---------------

   procedure Test_Tail is

      use type VSS.Strings.Virtual_String;

      S  : constant VSS.Strings.Virtual_String := "abcdefg";
      --  JF : VSS.Strings.Character_Iterators.Character_Iterator :=
      --    S.At_First_Character;
      --  JL : VSS.Strings.Character_Iterators.Character_Iterator :=
      --    S.At_Last_Character;
      JC : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.At_First_Character;

   begin
      --  Move iterator to the character inside the string.

      Test_Support.Assert (JC.Forward);
      Test_Support.Assert (JC.Forward);
      Test_Support.Assert (JC.Forward);

      Test_Support.Assert (S.Tail_From (JC) = "defg");
      Test_Support.Assert (S.Tail_After (JC) = "efg");

      --  Corner cases.

      Test_Support.Assert (S.Tail_From (S.At_First_Character) = "abcdefg");
      Test_Support.Assert (S.Tail_After (S.At_First_Character) = "bcdefg");

      Test_Support.Assert (S.Tail_From (S.At_Last_Character) = "g");
      Test_Support.Assert (S.Tail_After (S.At_Last_Character).Is_Empty);
   end Test_Tail;

begin
   Test_Tail;
end Test_String;
