--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Case conversion of UTF-8 encoded text data

with VSS.Implementation.UCD_Casing_UTF8;
with VSS.Implementation.UTF8_Encoding;
with VSS.Implementation.UTF8_Strings;
with VSS.Unicode;

package VSS.Implementation.UTF8_Casing is

   pragma Preelaborate;

   type Case_Mapping is
     (Simple_Lowercase,
      Simple_Titlecase,
      Simple_Uppercase,
      Simple_Case_Folding,
      NFKC_Casefold,
      Lowercase,
      Titlecase,
      Uppercase);

   procedure Get_Case_Mapping
     (Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Text    : out VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Fill given case mapping for the given character into Data.

   procedure Convert_Case
     (Text    : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Do case conversion of the string.

   procedure Convert_Case_Simple
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Result_Data    : in out
        VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Common code for simple case conversions.

   procedure Convert_Case_Full
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      To_Lower       : Boolean;
      Result_Data    : in out
        VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Common code for full case conversions.

   function Get_Simplified_Case_Mapping_Information
     (Mapping : VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Code    : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Casing_UTF8.Simplified_Mapping_Information;
   --  Returns case mapping information for given mapping and character.

   function Get_Contextual_Case_Mapping_Information
     (Mapping : VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Code    : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Casing_UTF8.Contextual_Mapping_Information;
   --  Returns case mapping information for given mapping and character.

end VSS.Implementation.UTF8_Casing;
