
with Ada.Containers.Vectors;
--  with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

--  with VSS.Strings.Character_Iterators;

with Test_U816_022;

procedure Main is

   package Test_Vectors is
     new Ada.Containers.Vectors
       (Positive, Test_U816_022.Test_Item, Test_U816_022."=");

   Test_Data : Test_Vectors.Vector;

   procedure Load;

   ----------
   -- Load --
   ----------

   procedure Load is
      File   : Ada.Wide_Wide_Text_IO.File_Type;
      Buffer : Wide_Wide_String (1 .. 1_024);
      Last   : Natural;
      Item   : Test_U816_022.Test_Item;

   begin
      Ada.Wide_Wide_Text_IO.Open
        (File, Ada.Wide_Wide_Text_IO.In_File, "tail.txt", "wcem=8");

      while not Ada.Wide_Wide_Text_IO.End_Of_File (File) loop
         Ada.Wide_Wide_Text_IO.Get_Line (File, Buffer, Last);
         Item.Offset := Integer'Wide_Wide_Value (Buffer (4 .. Last));

         Ada.Wide_Wide_Text_IO.Get_Line (File, Buffer, Last);
         Item.Text := new Wide_Wide_String'(Buffer (1 .. Last));

         Test_Data.Append (Item);
      end loop;

      Ada.Wide_Wide_Text_IO.Close (File);
   end Load;

   ----------
   -- Test --
   ----------

   procedure Test is
   begin
      for Item of Test_Data loop
         Test_U816_022.Run_Test_Iteration (Item);
      end loop;
   end Test;

begin
   Load;
   Test;
end Main;
