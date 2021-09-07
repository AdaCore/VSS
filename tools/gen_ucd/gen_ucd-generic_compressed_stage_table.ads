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

with Gen_UCD.Unsigned_Types;
with UCD;

generic
   type Data_Type is private;
   type Data_Type_Array is array (UCD.Code_Point) of Data_Type;

package Gen_UCD.Generic_Compressed_Stage_Table is

   Group_Size : constant := 256;

   type Group_Count is new Natural;
   subtype Group_Offset is Group_Count;

   type Data_Count is new Natural;
   subtype Data_Offset is Data_Count;

   type Compressed_Stage_Table is tagged limited private;

   procedure Build
     (Self : in out Compressed_Stage_Table'Class; Data : Data_Type_Array);

   function Index_Table_Last
     (Self : Compressed_Stage_Table'Class) return Group_Count;

   function Index_Table_Element
     (Self   : Compressed_Stage_Table'Class;
      Offset : Group_Offset) return Data_Offset;

   function Data_Table_Last return Data_Count;

   function Data_Table_Element (Offset : Data_Offset) return Data_Type;

private

   type Group_Array is
     array (Gen_UCD.Unsigned_Types.Unsigned_32 range <>)
       of Gen_UCD.Unsigned_Types.Unsigned_32;

   type Group_Array_Access is access all Group_Array;

   type Compressed_Stage_Table is tagged limited record
      Group_Data : Group_Array_Access;
   end record;

end Gen_UCD.Generic_Compressed_Stage_Table;
