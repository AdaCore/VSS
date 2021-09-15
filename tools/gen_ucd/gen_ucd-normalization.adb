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

pragma Ada_2022;

with Ada.Integer_Wide_Wide_Text_IO;     use Ada.Integer_Wide_Wide_Text_IO;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;       use Ada.Strings.Wide_Wide_Fixed;
with Ada.Unchecked_Conversion;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;

with UCD.Characters;
with UCD.Properties;

with Gen_UCD.Enumeration_Types;
with Gen_UCD.Compressed_UTF_8_Data;
with Gen_UCD.Generic_Compressed_Stage_Table;

package body Gen_UCD.Normalization is

   use type UCD.Properties.Property_Value_Access;

   type Decomposition_Kind is (Canonical, Compatibility);

   function Full_Decomposition
     (Code          : UCD.Code_Point;
      Decomposition : Decomposition_Kind;
      DT_Property   : not null UCD.Properties.Property_Access;
      DT_None       : not null UCD.Properties.Property_Value_Access;
      DT_Canonical  : not null UCD.Properties.Property_Value_Access;
      DM_Property   : not null UCD.Properties.Property_Access)
      return UCD.Code_Point_Vectors.Vector;

   CCC_Enumeration : Gen_UCD.Enumeration_Types.Enumeration_Type;

   package Database is

      procedure Initialize;

      procedure Set_Canonical_Decomposition
        (Character : UCD.Code_Point;
         Data      : UCD.Code_Point_Vectors.Vector);

      procedure Set_Compatibility_Decomposition
        (Character : UCD.Code_Point;
         Data      : UCD.Code_Point_Vectors.Vector);

      procedure Set_CCC
        (Character : UCD.Code_Point;
         To        : Natural);

      procedure Set_NFD_QC
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_NFKD_QC
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Compress;

      function UTF_8_Data_Index_Last return Gen_UCD.UTF_8_Offset;

      function UTF_8_Data_Element
        (Index : Gen_UCD.UTF_8_Offset) return Gen_UCD.UTF_8_Code_Unit;

      function Mapping_Data_Last return Natural;

      function Mapping_Data_Element (Offset : Natural) return Unsigned_64;

      function Mapping_Index_Last
        (Decomposition : Decomposition_Kind) return Natural;

      function Mapping_Index_Element
        (Decomposition : Decomposition_Kind;
         Offset        : Natural) return Natural;

      function Mapping_Index_Group_Size
        (Decomposition : Decomposition_Kind) return Natural;

      procedure Print_Statistics;

   end Database;

   -----------
   -- Build --
   -----------

   procedure Build is
      CCC_Property     : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("ccc");

      DT_Property      : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("dt");
      DT_None          : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (DT_Property, "None");
      DT_Canonical     : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (DT_Property, "Canonical");

      DM_Property      : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("dm");

      NFD_QC_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("NFD_QC");
      NFD_QC_Y         : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFD_QC_Property, "Y");

      NFKD_QC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("NFKD_QC");
      NFKD_QC_Y        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFKD_QC_Property, "Y");

   begin
      Put_Line ("   ... normalization");

      Database.Initialize;

      CCC_Enumeration.Initialize (CCC_Property);

      --  Process properties of each character. Do it in reverse order, it
      --  produce little bit smaller table.

      for Code in reverse UCD.Code_Point loop
         declare
            DT_Value      : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, DT_Property);
            NFD_QC_Value  : constant Boolean :=
              UCD.Characters.Get (Code, NFD_QC_Property) = NFD_QC_Y;
            NFKD_QC_Value : constant Boolean :=
              UCD.Characters.Get (Code, NFKD_QC_Property) = NFKD_QC_Y;

         begin
            if DT_Value = DT_None then
               null;

            elsif DT_Value = DT_Canonical then
               Database.Set_Canonical_Decomposition
                 (Code,
                  Full_Decomposition
                    (Code,
                     Canonical,
                     DT_Property, DT_None, DT_Canonical,
                     DM_Property));

            else
               Database.Set_Compatibility_Decomposition
                 (Code,
                  Full_Decomposition
                    (Code,
                     Compatibility,
                     DT_Property, DT_None, DT_Canonical,
                     DM_Property));
            end if;

            Database.Set_CCC (Code, CCC_Enumeration.Representation (Code));
            Database.Set_NFD_QC (Code, NFD_QC_Value);
            Database.Set_NFKD_QC (Code, NFKD_QC_Value);
         end;
      end loop;

      Database.Compress;

      Database.Print_Statistics;
   end Build;

   --------------
   -- Database --
   --------------

   package body Database is

      type Mapping_Record is record
         Decomposition_QC : Boolean     := True;
         CCC              : Unsigned_6  := 0;
         Offset           : Unsigned_14 := 0;
         Size             : Unsigned_6  := 0;
         Length           : Unsigned_5  := 0;
         Last_CCC         : Unsigned_6  := 0;
         Reserved_1       : Unsigned_2  := 0;
         Reserved_2       : Unsigned_2  := 0;
         Reserved_3       : Unsigned_3  := 0;
         Reserved_4       : Unsigned_2  := 0;
         Reserved_5       : Unsigned_17 := 0;
      end record;
      for Mapping_Record'Size use 64;
      for Mapping_Record use record
         Offset           at 0 range 0 .. 13;
         Reserved_1       at 0 range 14 .. 15;
         Size             at 0 range 16 .. 21;
         Reserved_2       at 0 range 22 .. 23;
         Length           at 0 range 24 .. 28;
         Reserved_3       at 0 range 29 .. 31;
         CCC              at 0 range 32 .. 37;
         Reserved_4       at 0 range 38 .. 39;
         Last_CCC         at 0 range 40 .. 45;
         Reserved_5       at 0 range 46 .. 62;
         Decomposition_QC at 0 range 63 .. 63;
      end record;
      --  This declaration must be synchronized with type declaration in the
      --  generated code.
      --
      --  This record contains additional information that may be derived or
      --  be copy of core properties when is in interest of normalization
      --  algoriphms. This not increase total amount of the data for UCD, but
      --  allows to have all necessary data in one place, primary to mininize
      --  CPU cache usage.

      type Mapping_Array is array (UCD.Code_Point) of Mapping_Record;

      type Mapping_Array_Access is access all Mapping_Array;

      Raw_Mapping : array (Decomposition_Kind) of Mapping_Array_Access;

      Max_Length  : Natural := 0;
      Max_UTF_8   : Natural := 0;
      Total_UTF_8 : Natural := 0;

      package Compressed_Stage_Table is
        new Gen_UCD.Generic_Compressed_Stage_Table
              (Mapping_Record, Mapping_Array);

      Compressed :
        array (Decomposition_Kind)
          of Compressed_Stage_Table.Compressed_Stage_Table;

      UTF_8_Data : Gen_UCD.Compressed_UTF_8_Data.Compressed_UTF_8_Data;

      --------------
      -- Compress --
      --------------

      procedure Compress is
      begin
         for J in Decomposition_Kind'Range loop
            Compressed (J).Build (Raw_Mapping (J).all);
         end loop;
      end Compress;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         for D in Decomposition_Kind'Range loop
            Raw_Mapping (D) := new Mapping_Array;
         end loop;
      end Initialize;

      --------------------------
      -- Mapping_Data_Element --
      --------------------------

      function Mapping_Data_Element (Offset : Natural) return Unsigned_64 is
         function To_Unsigned_64 is
            new Ada.Unchecked_Conversion (Mapping_Record, Unsigned_64);

      begin
         return
           To_Unsigned_64
             (Compressed_Stage_Table.Data_Table_Element
                (Compressed_Stage_Table.Data_Count (Offset)));
      end Mapping_Data_Element;

      -----------------------
      -- Mapping_Data_Last --
      -----------------------

      function Mapping_Data_Last return Natural is
      begin
         return Natural (Compressed_Stage_Table.Data_Table_Last);
      end Mapping_Data_Last;

      ---------------------------
      -- Mapping_Index_Element --
      ---------------------------

      function Mapping_Index_Element
        (Decomposition : Decomposition_Kind;
         Offset        : Natural) return Natural is
      begin
         return
           Natural
             (Compressed (Decomposition).Index_Table_Element
              (Compressed_Stage_Table.Group_Offset (Offset)));
      end Mapping_Index_Element;

      ------------------------------
      -- Mapping_Index_Group_Size --
      ------------------------------

      function Mapping_Index_Group_Size
        (Decomposition : Decomposition_Kind) return Natural
      is
         pragma Unreferenced (Decomposition);

      begin
         return Compressed_Stage_Table.Group_Size;
      end Mapping_Index_Group_Size;

      ------------------------
      -- Mapping_Index_Last --
      ------------------------

      function Mapping_Index_Last
        (Decomposition : Decomposition_Kind) return Natural is
      begin
         return Natural (Compressed (Decomposition).Index_Table_Last);
      end Mapping_Index_Last;

      ----------------------
      -- Print_Statistics --
      ----------------------

      procedure Print_Statistics is
      begin
         Put_Line
           ("         max expanded length is"
            & Natural'Wide_Wide_Image (Max_Length)
            & ", max UTF-8 sequence is"
            & Natural'Wide_Wide_Image (Max_UTF_8));

         Put_Line
           ("         UTF-8 data size is"
            & Natural'Wide_Wide_Image (Total_UTF_8)
            & " bytes (compressed size is"
            & Natural'Wide_Wide_Image (Natural (UTF_8_Data.Last_Index) + 1)
            & " bytes)");
      end Print_Statistics;

      ---------------------------------
      -- Set_Canonical_Decomposition --
      ---------------------------------

      procedure Set_Canonical_Decomposition
        (Character : UCD.Code_Point;
         Data      : UCD.Code_Point_Vectors.Vector)
      is
         Last_CCC : constant Unsigned_6 :=
           Unsigned_6 (CCC_Enumeration.Representation (Data.Last_Element));

         Offset   : UTF_8_Offset;
         Size     : UTF_8_Count;
         Length   : Natural;

      begin
         UTF_8_Data.Append_Data (Data, Offset, Size, Length);

         Raw_Mapping (Canonical) (Character).Offset   := Unsigned_14 (Offset);
         Raw_Mapping (Canonical) (Character).Size     := Unsigned_6 (Size);
         Raw_Mapping (Canonical) (Character).Length   := Unsigned_5 (Length);
         Raw_Mapping (Canonical) (Character).Last_CCC := Last_CCC;

         Raw_Mapping (Compatibility) (Character).Offset :=
           Unsigned_14 (Offset);
         Raw_Mapping (Compatibility) (Character).Size   :=
           Unsigned_6 (Size);
         Raw_Mapping (Compatibility) (Character).Length :=
           Unsigned_5 (Length);
         Raw_Mapping (Compatibility) (Character).Last_CCC := Last_CCC;

         Max_Length  := Natural'Max (Max_Length, Length);
         Max_UTF_8   := Natural'Max (Max_UTF_8, Natural (Size));
         Total_UTF_8 := Total_UTF_8 + Natural (Size);
      end Set_Canonical_Decomposition;

      -------------
      -- Set_CCC --
      -------------

      procedure Set_CCC
        (Character : UCD.Code_Point;
         To        : Natural) is
      begin
         Raw_Mapping (Canonical) (Character).CCC := Unsigned_6 (To);
         Raw_Mapping (Compatibility) (Character).CCC := Unsigned_6 (To);
      end Set_CCC;

      -------------------------------------
      -- Set_Compatibility_Decomposition --
      -------------------------------------

      procedure Set_Compatibility_Decomposition
        (Character : UCD.Code_Point;
         Data      : UCD.Code_Point_Vectors.Vector)
      is
         Offset : UTF_8_Offset;
         Size   : UTF_8_Count;
         Length : Natural;

      begin
         UTF_8_Data.Append_Data (Data, Offset, Size, Length);

         Raw_Mapping (Compatibility) (Character).Offset :=
           Unsigned_14 (Offset);
         Raw_Mapping (Compatibility) (Character).Size   :=
           Unsigned_6 (Size);
         Raw_Mapping (Compatibility) (Character).Length :=
           Unsigned_5 (Length);

         Max_Length  := Natural'Max (Max_Length, Length);
         Max_UTF_8   := Natural'Max (Max_UTF_8, Natural (Size));
         Total_UTF_8 := Total_UTF_8 + Natural (Size);
      end Set_Compatibility_Decomposition;

      ----------------
      -- Set_NFD_QC --
      ----------------

      procedure Set_NFD_QC
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         Raw_Mapping (Canonical) (Character).Decomposition_QC := To;
      end Set_NFD_QC;

      -----------------
      -- Set_NFKD_QC --
      -----------------

      procedure Set_NFKD_QC
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         Raw_Mapping (Compatibility) (Character).Decomposition_QC := To;
      end Set_NFKD_QC;

      ------------------------
      -- UTF_8_Data_Element --
      ------------------------

      function UTF_8_Data_Element
        (Index : Gen_UCD.UTF_8_Offset) return Gen_UCD.UTF_8_Code_Unit is
      begin
         return UTF_8_Data.Element (Index);
      end UTF_8_Data_Element;

      ---------------------------
      -- UTF_8_Data_Index_Last --
      ---------------------------

      function UTF_8_Data_Index_Last return Gen_UCD.UTF_8_Offset is
      begin
         return UTF_8_Data.Last_Index;
      end UTF_8_Data_Index_Last;

   end Database;

   ------------------------
   -- Full_Decomposition --
   ------------------------

   function Full_Decomposition
     (Code          : UCD.Code_Point;
      Decomposition : Decomposition_Kind;
      DT_Property   : not null UCD.Properties.Property_Access;
      DT_None       : not null UCD.Properties.Property_Value_Access;
      DT_Canonical  : not null UCD.Properties.Property_Value_Access;
      DM_Property   : not null UCD.Properties.Property_Access)
      return UCD.Code_Point_Vectors.Vector
   is
      DT_Value : constant not null UCD.Properties.Property_Value_Access :=
        UCD.Characters.Get (Code, DT_Property);
      DM_Value : constant UCD.Properties.Property_Value_Access :=
        UCD.Characters.Get (Code, DM_Property);

   begin
      return Result : UCD.Code_Point_Vectors.Vector do
         if DT_Value = DT_None then
            Result.Append (Code);

         elsif DT_Value = DT_Canonical then
            for C of DM_Value.String loop
               Result.Append
                 (Full_Decomposition
                    (C,
                     Decomposition,
                     DT_Property, DT_None, DT_Canonical,
                     DM_Property));
            end loop;

         else
            if Decomposition = Canonical then
               Result.Append (Code);

            else
               for C of DM_Value.String loop
                  Result.Append
                    (Full_Decomposition
                       (C,
                        Decomposition,
                        DT_Property, DT_None, DT_Canonical,
                        DM_Property));
               end loop;
            end if;
         end if;
      end return;
   end Full_Decomposition;

   --------------
   -- Generate --
   --------------

   procedure Generate (File : Ada.Wide_Wide_Text_IO.File_Type) is

      procedure Generate_Index_Table
        (Decomposition : Decomposition_Kind;
         Name          : Wide_Wide_String);

      --------------------------
      -- Generate_Index_Table --
      --------------------------

      procedure Generate_Index_Table
        (Decomposition : Decomposition_Kind;
         Name          : Wide_Wide_String)
      is
         Image : Wide_Wide_String (1 .. 8);

      begin
         Put_Line (File, "   " & Name & " :");
         Put_Line (File, "     constant Mapping_Data_Offset_Array :=");

         for J in 0 .. Database.Mapping_Index_Last (Decomposition) loop
            Put (Image, Database.Mapping_Index_Element (Decomposition, J));

            if J = 0 then
               Put (File, "       (");

            elsif J mod 10 = 0 then
               Put_Line (File, ",");
               Put (File, "        ");

            else
               Put (File, ", ");
            end if;

            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, ");");
         New_Line (File);
      end Generate_Index_Table;

   begin
      Put_Line ("   ... normalization");

      Put_Line (File, "pragma Restrictions (No_Elaboration_Code);");
      New_Line (File);

      Put_Line (File, "with VSS.Implementation.Strings;");
      Put_Line (File, "with VSS.Implementation.UTF8_Encoding;");
      Put_Line (File, "with VSS.Unicode;");
      Put_Line (File, "with Interfaces;");
      New_Line (File);
      Put_Line (File, "package VSS.Implementation.UCD_Normalization_UTF8 is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      New_Line (File);

      --  Generate CCC enumeration type declaration.

      CCC_Enumeration.Generate_Type_Declaration (File);

      --  Generate data types

      Put_Line
        (File,
         "   subtype Normalization_UTF8_Data_Offset is");
      Put_Line
        (File,
         "     VSS.Unicode.UTF8_Code_Unit_Offset range 0 .."
         & Positive'Wide_Wide_Image (Positive (Database.UTF_8_Data_Index_Last))
         & ";");
      New_Line (File);

      Put_Line
        (File,
         "   subtype Normalization_Character_Count is");
      Put_Line
        (File,
         "     VSS.Implementation.Strings.Character_Count range 0 .. 18;");
      New_Line (File);

      Put_Line
        (File,
         "   subtype Normalization_UTF8_Code_Unit_Count is");
      Put_Line
        (File,
         "     VSS.Unicode.UTF8_Code_Unit_Count range 0 .. 33;");
      New_Line (File);

      Put_Line
        (File,
         "   type Mapping_Information is record");
      Put_Line (File, "      Decomposition_QC : Boolean;");
      Put_Line (File, "      CCC              : CCC_Values;");
      Put_Line
        (File, "      Offset           : Normalization_UTF8_Data_Offset;");
      Put_Line
        (File, "      Size             : Normalization_UTF8_Code_Unit_Count;");
      Put_Line
        (File, "      Length           : Normalization_Character_Count;");
      Put_Line (File, "      Last_CCC         : CCC_Values;");
      Put_Line (File, "   end record;");
      Put_Line
        (File,
         "   for Mapping_Information'Size use 64;");
      Put_Line (File, "   for Mapping_Information use record");
      Put_Line (File, "      Offset           at 0 range 0 .. 13;");
      Put_Line (File, "      Size             at 0 range 16 .. 21;");
      Put_Line (File, "      Length           at 0 range 24 .. 28;");
      Put_Line (File, "      CCC              at 0 range 32 .. 37;");
      Put_Line (File, "      Last_CCC         at 0 range 40 .. 45;");
      Put_Line (File, "      Decomposition_QC at 0 range 63 .. 63;");
      Put_Line (File, "   end record;");
      New_Line (File);

      Put_Line
        (File,
         "   Mapping_Group_Size : constant :="
         & Integer'Wide_Wide_Image
             (Database.Mapping_Index_Group_Size (Canonical))
         & ";");
      New_Line (File);

      Put_Line
        (File,
         "   type Mapping_Group is range 0 .."
         & Integer'Wide_Wide_Image (Database.Mapping_Index_Last (Canonical))
         & ";");
      New_Line (File);

      Put_Line
        (File,
         "   type Mapping_Data_Offset is range 0 .."
         & Integer'Wide_Wide_Image (Database.Mapping_Data_Last)
         & ";");
      New_Line (File);

      Put_Line (File, "   type Mapping_Data_Offset_Array is");
      Put_Line (File, "     array (Mapping_Group) of Mapping_Data_Offset;");
      New_Line (File);

      --  Generate normalization indices.

      Generate_Index_Table (Canonical, "Canonical_Index");
      Generate_Index_Table (Compatibility, "Compatibility_Index");

      --  Generate normalization data table.

      declare

         package Mapping_Data_IO is
           new Ada.Wide_Wide_Text_IO.Modular_IO (Unsigned_64);
         use Mapping_Data_IO;

         Image : Wide_Wide_String (1 .. 20);

      begin
         Put_Line
           (File,
            "   Mapping_Data_Table_Raw :");
         Put_Line
           (File,
            "     constant array (Mapping_Data_Offset)");
         Put_Line
           (File,
            "       of Interfaces.Unsigned_64 :=");

         for J in 0 .. Database.Mapping_Data_Last loop
            Put (Image, Database.Mapping_Data_Element (J), 16);

            if J = 0 then
               Put (File, "         (");

            elsif J mod 3 = 0 then
               Put_Line (File, ",");
               Put (File, "          ");

            else
               Put (File, ", ");
            end if;

            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, ");");
         New_Line (File);

         Put_Line (File, "   Mapping_Data_Table :");
         Put_Line
           (File,
            "     constant array (Mapping_Data_Offset)"
            & " of Mapping_Information");
         Put_Line (File, "       with Import,");
         Put_Line (File, "            Convention => Ada,");
         Put_Line
           (File,
            "            Address    => Mapping_Data_Table_Raw'Address;");
         New_Line (File);
      end;

      --  Generate UTF-8 encoded data.

      declare
         Image : Wide_Wide_String (1 .. 10);

      begin
         Put_Line
           (File,
            "   UTF8_Data_Table :");
         Put_Line
           (File,
            "     constant"
              & " VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array");
         Put_Line
           (File,
            "       (Normalization_UTF8_Data_Offset) :=");

         for J in 0 .. Database.UTF_8_Data_Index_Last loop
            Put (Image, Integer (Database.UTF_8_Data_Element (J)), 16);

            if J = 0 then
               Put (File, "         (");

            elsif J mod 8 = 0 then
               Put_Line (File, ",");
               Put (File, "          ");

            else
               Put (File, ", ");
            end if;

            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, ");");
         New_Line (File);
      end;

      Put_Line (File, "end VSS.Implementation.UCD_Normalization_UTF8;");
   end Generate;

end Gen_UCD.Normalization;
