--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Unicode normalization for UTF8 encoded text data

with VSS.Implementation.UCD_Normalization_UTF8;
with VSS.Implementation.UTF8_Encoding;
with VSS.Implementation.UTF8_Strings;
with VSS.Unicode;

package VSS.Implementation.UTF8_Normalization is

   pragma Preelaborate;

   procedure Normalize
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Form   : VSS.Implementation.Normalization_Form;
      Result : out VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Do normalization of the string to the given normalization form.

   procedure Decompose
     (Source_Storage     :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Result_Data        : out
        VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Common code to decomposite string according to given decomposition
   --  mapping (canonical or compatibility) and to do canonical reordering.

   procedure Decompose_And_Compose
     (Source_Storage     :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Result_Data        : out
        VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Common code to decompose string according to given decomposition
   --  mapping (canonical or compatibility), to do canonical reordering, and
   --  to canonically compose.

end VSS.Implementation.UTF8_Normalization;
