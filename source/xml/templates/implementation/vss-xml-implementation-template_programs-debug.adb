--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Wide_Wide_Text_IO;
with VSS.Strings.Conversions;

package body VSS.XML.Implementation.Template_Programs.Debug is

   package Address_IO is
     new Ada.Wide_Wide_Text_IO.Integer_IO (Address);

   package Instruction_Kind_IO is
     new Ada.Wide_Wide_Text_IO.Enumeration_IO (Instruction_Kind);

   ------------------
   -- Dump_Program --
   ------------------

   procedure Dump_Program (Program : Instruction_Vectors.Vector) is
   begin
      for J in Program.First_Index .. Program.Last_Index loop
         declare
            Item : Instruction renames Program (J);

         begin
            Address_IO.Put (J, Width => 4);
            Ada.Wide_Wide_Text_IO.Put (' ');
            Instruction_Kind_IO.Put (Item.Kind);

            case Item.Kind is
               when None =>
                  null;

               when Start_Element =>
                  Ada.Wide_Wide_Text_IO.Put
                    (" '"
                     & VSS.Strings.Conversions.To_Wide_Wide_String
                       (Item.URI.To_Virtual_String)
                     & "':"
                     & VSS.Strings.Conversions.To_Wide_Wide_String
                       (Item.Name));

               when Attribute =>
                  null;

               when End_Element =>
                  Address_IO.Put (Item.Start_Address);

               when Text =>
                  null;

               when Comment =>
                  null;

               when Processing_Instruction =>
                  null;

               when Location =>
                  null;

               when Condition =>
                  null;

               when Content =>
                  null;

               when Omit_Tag =>
                  null;

               when Repeat =>
                  null;

               when Done =>
                  null;
            end case;

            Ada.Wide_Wide_Text_IO.New_Line;
         end;
      end loop;
   end Dump_Program;

end VSS.XML.Implementation.Template_Programs.Debug;
