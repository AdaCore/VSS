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

with Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with UCD.Characters;
with UCD.Derived_Core_Properties_Loader;
with UCD.Properties;
with UCD.Property_Aliases_Loader;
with UCD.Property_Value_Aliases_Loader;

with VSS.Characters;

with Test_Support;

procedure Test_Characters is
   use type VSS.Characters.General_Category;

   procedure Initialize_UCD;
   --  Initialize UCD and loads necessary information for testing.

   procedure Test_Properties;
   --  Test properties of all characters.

   --------------------
   -- Initialize_UCD --
   --------------------

   procedure Initialize_UCD is
   begin
      if Ada.Command_Line.Argument_Count /= 1 then
         raise Program_Error;
      end if;

      declare
         UCD_Root : constant Wide_Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
             (Ada.Command_Line.Argument (1));

      begin
         UCD.Property_Aliases_Loader.Load (UCD_Root);
         UCD.Property_Value_Aliases_Loader.Load (UCD_Root);

         UCD.Characters.Initialize_Character_Database;

         UCD.Derived_Core_Properties_Loader.Load (UCD_Root);
      end;
   end Initialize_UCD;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      use type UCD.Properties.Property_Value_Access;

      Lowercase_Property : constant UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("Lowercase");
      Lowercase_Y        : constant UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (Lowercase_Property, "Y");
      Lowercase_N        : constant UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (Lowercase_Property, "N");
      Uppercase_Property : constant UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("Uppercase");
      Uppercase_Y        : constant UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (Uppercase_Property, "Y");
      Uppercase_N        : constant UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (Uppercase_Property, "N");
      Cased_Property     : constant UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("Cased");
      Cased_Y            : constant UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (Cased_Property, "Y");
      Cased_N            : constant UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (Cased_Property, "N");

   begin
      for Character in VSS.Characters.Virtual_Character'Range loop
         if VSS.Characters.Get_Lowercase (Character) then
            Test_Support.Assert
              (UCD.Characters.Get
                 (VSS.Characters.Virtual_Character'Pos (Character),
                  Lowercase_Property) = Lowercase_Y);

         else
            Test_Support.Assert
              (UCD.Characters.Get
                 (VSS.Characters.Virtual_Character'Pos (Character),
                  Lowercase_Property) = Lowercase_N);
         end if;

         if VSS.Characters.Get_Uppercase (Character) then
            Test_Support.Assert
              (UCD.Characters.Get
                 (VSS.Characters.Virtual_Character'Pos (Character),
                  Uppercase_Property) = Uppercase_Y);

         else
            Test_Support.Assert
              (UCD.Characters.Get
                 (VSS.Characters.Virtual_Character'Pos (Character),
                  Uppercase_Property) = Uppercase_N);
         end if;

         if VSS.Characters.Get_Cased (Character) then
            Test_Support.Assert
              (UCD.Characters.Get
                 (VSS.Characters.Virtual_Character'Pos (Character),
                  Cased_Property) = Cased_Y);

         else
            Test_Support.Assert
              (UCD.Characters.Get
                 (VSS.Characters.Virtual_Character'Pos (Character),
                  Cased_Property) = Cased_N);
         end if;
      end loop;
   end Test_Properties;

begin
   Initialize_UCD;

   --  This test can be replaces by the test with full coverage of possible
   --  Unicode characters listed in 'extracted/DerivedGeneralCategory.txt'
   --  file of UCD.

   Test_Support.Assert
     (VSS.Characters.Get_General_Category
        (VSS.Characters.Virtual_Character'Val (16#00#))
      = VSS.Characters.Control);
   Test_Support.Assert
     (VSS.Characters.Get_General_Category
        (VSS.Characters.Virtual_Character'Val (16#20#))
      = VSS.Characters.Space_Separator);
   Test_Support.Assert
     (VSS.Characters.Get_General_Category
        (VSS.Characters.Virtual_Character'Val (16#31#))
      = VSS.Characters.Decimal_Number);
   Test_Support.Assert
     (VSS.Characters.Get_General_Category
        (VSS.Characters.Virtual_Character'Val (16#41#))
      = VSS.Characters.Uppercase_Letter);
   Test_Support.Assert
     (VSS.Characters.Get_General_Category
        (VSS.Characters.Virtual_Character'Val (16#430#))
      = VSS.Characters.Lowercase_Letter);
   Test_Support.Assert
     (VSS.Characters.Get_General_Category
        (VSS.Characters.Virtual_Character'Val (16#D800#))
      = VSS.Characters.Surrogate);

   Test_Properties;
end Test_Characters;
