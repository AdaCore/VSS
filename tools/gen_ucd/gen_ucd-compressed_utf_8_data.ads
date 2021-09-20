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

private with Ada.Containers.Vectors;

with UCD;

package Gen_UCD.Compressed_UTF_8_Data is

   type Compressed_UTF_8_Data is tagged limited private;

   procedure Append_Data
     (Self   : in out Compressed_UTF_8_Data;
      Data   : UCD.Code_Point_Vectors.Vector;
      Offset : out Gen_UCD.UTF_8_Offset;
      Size   : out Gen_UCD.UTF_8_Count;
      Length : out Natural);

   function Element
     (Self   : Compressed_UTF_8_Data;
      Offset : Gen_UCD.UTF_8_Offset) return Gen_UCD.UTF_8_Code_Unit;

   function Last_Index
     (Self : Compressed_UTF_8_Data) return Gen_UCD.UTF_8_Offset;

private

   package UTF_8_Code_Unit_Vectors is
     new Ada.Containers.Vectors (Gen_UCD.UTF_8_Count, Gen_UCD.UTF_8_Code_Unit);

   type Compressed_UTF_8_Data is tagged limited record
      Data : UTF_8_Code_Unit_Vectors.Vector;
   end record;

end Gen_UCD.Compressed_UTF_8_Data;
