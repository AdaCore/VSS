--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Case conversion of UTF-8 encoded text data

with VSS.Implementation.Strings;
with VSS.Implementation.String_Handlers;
with VSS.Implementation.UCD_Casing_UTF8;
with VSS.Implementation.UTF8_Encoding;
with VSS.Unicode;

package VSS.Implementation.UTF8_Casing is

   pragma Preelaborate;

   procedure Convert_Case
     (Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.String_Handlers.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data);
   --  Do case conversion of the string.

   procedure Convert_Case_Simple
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Result_Data    : in out VSS.Implementation.Strings.String_Data);
   --  Common code for simple case conversions.

   procedure Convert_Case_Full
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      To_Lower       : Boolean;
      Result_Data    : in out VSS.Implementation.Strings.String_Data);
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
