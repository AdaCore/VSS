--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body UCD.Properties is

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Property_Value_Access) return Ada.Containers.Hash_Type is
   begin
      return Wide_Wide_Hash (Item.Names.First_Element);
   end Hash;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Property_Name : Wide_Wide_String) return not null Property_Access is
   begin
      return
        Name_To_Property.Element
          (To_Unbounded_Wide_Wide_String (Property_Name));
   end Resolve;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Property   : not null Property_Access;
      Value_Name : Wide_Wide_String) return Property_Value_Access is
   begin
      if Property.Is_Canonical_Combining_Class then
         begin
            declare
               Value : constant Canonical_Combinig_Class :=
                 Canonical_Combinig_Class'Wide_Wide_Value (Value_Name);

            begin
               for V of Property.All_Values loop
                  if V.Canonical_Combining_Class_Value = Value then
                     return V;
                  end if;
               end loop;

               raise Program_Error;
            end;

         exception
            when Constraint_Error =>
               return
                 Property.Name_To_Value
                   (To_Unbounded_Wide_Wide_String (Value_Name));
         end;

      else
         return
           Property.Name_To_Value (To_Unbounded_Wide_Wide_String (Value_Name));
      end if;
   end Resolve;

end UCD.Properties;
