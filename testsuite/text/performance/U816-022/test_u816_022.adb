
with VSS.Strings.Character_Iterators;

package body Test_U816_022 is

   ------------------------
   -- Run_Test_Iteration --
   ------------------------

   procedure Run_Test_Iteration (Item : Test_Item) is
      S : VSS.Strings.Virtual_String :=
        VSS.Strings.To_Virtual_String (Item.Text.all);
      J : VSS.Strings.Character_Iterators.Character_Iterator :=
        S.First_Character;
      D : Boolean with Unreferenced;
      R : VSS.Strings.Virtual_String;

   begin
      for K in 1 .. Item.Offset loop
         if not J.Forward then
            raise Program_Error;
         end if;
      end loop;

      R := S.Slice (J, S.Last_Character);
   end Run_Test_Iteration;

end Test_U816_022;
