------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

package body Gen_UCD.Properties is

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

      else
         return
           Property.Name_To_Value (To_Unbounded_Wide_Wide_String (Value_Name));
      end if;
   end Resolve;

end Gen_UCD.Properties;
