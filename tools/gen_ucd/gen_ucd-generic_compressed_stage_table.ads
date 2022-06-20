--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with UCD;

generic
   type Data_Type is private;
   type Data_Type_Array is array (UCD.Code_Point) of Data_Type;

   with function "="
     (Left : Data_Type; Right : Data_Type) return Boolean is <>;
   --  Function to check equality of two items, it is necessary to be able
   --  to use Unchecked_Union types for Data_Type, because default
   --  implementation of "=" for them raise exception.

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
     array (Gen_UCD.Unsigned_32 range <>) of Gen_UCD.Unsigned_32;

   type Group_Array_Access is access all Group_Array;

   type Compressed_Stage_Table is tagged limited record
      Group_Data : Group_Array_Access;
   end record;

end Gen_UCD.Generic_Compressed_Stage_Table;
