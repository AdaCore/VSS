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

with Ada.Strings.Unbounded;

separate (Test_Stream_Element_Buffer)

procedure Test_Conversions is
   U : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("ABC123");
   V : constant VSS.Stream_Element_Vectors.Stream_Element_Vector :=
     VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
       (U);
   S : constant String :=
     VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String (V);

begin
   Test_Support.Assert (S = Ada.Strings.Unbounded.To_String (U));
end Test_Conversions;
