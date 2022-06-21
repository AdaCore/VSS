--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
