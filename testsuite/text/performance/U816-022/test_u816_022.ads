
with Ada.Strings.Wide_Wide_Unbounded;

package Test_U816_022 is

   type Test_Item is record
      Offset : Natural;
      Text   : Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_String_Access;
   end record;

   procedure Run_Test_Iteration (Item : Test_Item);

end Test_U816_022;
