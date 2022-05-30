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

package body JSON_Schema is

   --------------
   -- Is_False --
   --------------

   function Is_False (Self : Schema'Class) return Boolean is
   begin
      return Self.Negate /= null and then Self.Negate.Is_True;
   end Is_False;

   -------------
   -- Is_True --
   -------------

   function Is_True (Self : Schema'Class) return Boolean is
   begin
      return Self.Additional_Items = null
        and Self.Items.Is_Empty
        and Self.Additional_Properties = null
        and Self.Properties.Is_Empty
        and Self.Pattern_Properties.Is_Empty
        and Self.Property_Names = null
        and Self.Const.Is_Empty
        and Self.Enum.Is_Empty
        and Self.Kind.Is_Empty
        and Self.If_Schema = null
        and Self.All_Of.Is_Empty
        and Self.Any_Of.Is_Empty
        and Self.One_Of.Is_Empty
        and Self.Negate = null;
   end Is_True;

end JSON_Schema;