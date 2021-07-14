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

with VSS.Characters;

with Test_Support;

procedure Test_Characters is
   use type VSS.Characters.General_Category;

begin
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
end Test_Characters;
