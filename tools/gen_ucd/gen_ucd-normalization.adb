--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Containers;
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

   type Composition_Quick_Check is (No, Maybe, Yes);

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
      --  Sets CCC/Last_CCC/First_CCC members of the record. Need to be called
      --  before Set_Canonical_Decomposition/Set_Compatibility_Decomposition
      --  because they set Last_CCC/First_CCC members.

      procedure Set_NFD_QC
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_NFC_QC
        (Character : UCD.Code_Point;
         To        : Composition_Quick_Check);

      procedure Set_NFKD_QC
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_NFKC_QC
        (Character : UCD.Code_Point;
         To        : Composition_Quick_Check);

      procedure Set_NFC_Has_Starter
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_NFKC_Has_Starter
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Register_First_Code (Character : UCD.Code_Point);

      procedure Register_Last_Code (Character : UCD.Code_Point);

      procedure Set_Composition_Mapping
        (First   : UCD.Code_Point;
         Last    : UCD.Code_Point;
         Primary : UCD.Code_Point);

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

      function Composition_First_Index_Last return Natural;

      function Composition_Last_Index_Last return Natural;

      function Composition_Data_Element
        (First : Natural; Last : Natural) return UCD.Code_Point;

      procedure Print_Statistics;

   end Database;

   -----------
   -- Build --
   -----------

   procedure Build is
      CCC_Property     : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("ccc");
      CCC_NR           : constant UCD.Properties.Property_Value_Access :=
        UCD.Properties.Resolve (CCC_Property, "0");

      Comp_Ex_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("Comp_Ex");
      Comp_Ex_N        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (Comp_Ex_Property, "N");

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

      NFC_QC_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("NFC_QC");
      NFC_QC_Y         : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFC_QC_Property, "Y");
      NFC_QC_M         : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFC_QC_Property, "M");

      NFKD_QC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("NFKD_QC");
      NFKD_QC_Y        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFKD_QC_Property, "Y");

      NFKC_QC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("NFKC_QC");
      NFKC_QC_Y        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFKC_QC_Property, "Y");
      NFKC_QC_M        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFKC_QC_Property, "M");

      type Boolean_Array is array (UCD.Code_Point) of Boolean with Pack;

      Is_Primary_Composite : Boolean_Array := (others => False);
      Is_First_Code        : Boolean_Array := (others => False);
      Is_Last_Code         : Boolean_Array := (others => False);
      --  Code point is first or last code point of decomposition mapping of
      --  primary composite character.

   begin
      Put_Line ("   ... normalization");

      Database.Initialize;

      CCC_Enumeration.Initialize (CCC_Property);

      --  Process properties of each character. Do it in reverse order, it
      --  produce little bit smaller table.
      --
      --  Also, do first step of analysis of information for canonical
      --  composition: prepare list of characters that is primary composite,
      --  first or last characters of canonical decomposition of primary
      --  composite characters.

      for Code in reverse UCD.Code_Point loop
         declare
            use type Ada.Containers.Count_Type;

            Comp_Ex_Value : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, Comp_Ex_Property);

            DT_Value      : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, DT_Property);
            DM_Value      : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, DM_Property);
            NFD_QC_Value  : constant Boolean :=
              UCD.Characters.Get (Code, NFD_QC_Property) = NFD_QC_Y;
            NFC_QC_Value  : constant Composition_Quick_Check :=
              (if UCD.Characters.Get (Code, NFC_QC_Property) = NFC_QC_Y
               then Yes
               elsif UCD.Characters.Get (Code, NFC_QC_Property) = NFC_QC_M
               then Maybe
               else No);
            NFKD_QC_Value : constant Boolean :=
              UCD.Characters.Get (Code, NFKD_QC_Property) = NFKD_QC_Y;
            NFKC_QC_Value : constant Composition_Quick_Check :=
              (if UCD.Characters.Get (Code, NFKC_QC_Property) = NFKC_QC_Y
               then Yes
               elsif UCD.Characters.Get (Code, NFKC_QC_Property) = NFKC_QC_M
               then Maybe
               else No);

         begin
            Database.Set_CCC (Code, CCC_Enumeration.Representation (Code));
            Database.Set_NFD_QC (Code, NFD_QC_Value);
            Database.Set_NFC_QC (Code, NFC_QC_Value);
            Database.Set_NFKD_QC (Code, NFKD_QC_Value);
            Database.Set_NFKC_QC (Code, NFKC_QC_Value);

            --  Process information for canonical composition

            if DT_Value = DT_Canonical and then Comp_Ex_Value = Comp_Ex_N then
               if DM_Value.String.Length /= 2 then
                  --  Decomposition mapping must have two elements as of
                  --  Unicode 13

                  raise Program_Error;
               end if;

               Is_Primary_Composite (Code)                   := True;
               Is_First_Code (DM_Value.String.First_Element) := True;
               Is_Last_Code (DM_Value.String.Last_Element)   := True;
            end if;

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
               Database.Set_Compatibility_Decomposition
                 (Code,
                  Full_Decomposition
                    (Code,
                     Compatibility,
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
         end;
      end loop;

      --  Register characters that is first or last characters of the
      --  canonical decomposition mapping of primary composite characters.

      for Code in UCD.Code_Point loop
         if Is_First_Code (Code) then
            Database.Register_First_Code (Code);
         end if;

         if Is_Last_Code (Code) then
            Database.Register_Last_Code (Code);
         end if;
      end loop;

      --  Register mappings for composition

      for Code in UCD.Code_Point loop
         if Is_Primary_Composite (Code) then
            declare
               DM_Value : constant UCD.Properties.Property_Value_Access :=
                 UCD.Characters.Get (Code, DM_Property);

            begin
               Database.Set_Composition_Mapping
                 (DM_Value.String.First_Element,
                  DM_Value.String.Last_Element,
                  Code);
            end;
         end if;
      end loop;

      for Code in UCD.Code_Point loop
         declare

            function Has_Starter
              (CCC_Value : UCD.Properties.Property_Value_Access;
               Mapping   : UCD.Code_Point_Vectors.Vector) return Boolean;
            --  Returns True when character has starters in the full
            --  decomposition mapping.

            -----------------
            -- Has_Starter --
            -----------------

            function Has_Starter
              (CCC_Value : UCD.Properties.Property_Value_Access;
               Mapping   : UCD.Code_Point_Vectors.Vector) return Boolean
            is
               Mapping_Length : constant Natural := Natural (Mapping.Length);
               Found          : Boolean          := False;

            begin
               if Mapping.Is_Empty then
                  --  No mapping, check whether character is starter

                  --  ???? Last character of pair of primary character
                  --  decomposition ????

                  Found := CCC_Value = CCC_NR;

               elsif CCC_Value = CCC_NR then
                  --  Starter, but it has decomposition.

                  --  Lookup for starter character inside full decomposition
                  --  mapping.

                  for J in 1 .. Mapping_Length loop
                     declare
                        J_CCC_Value :
                          constant UCD.Properties.Property_Value_Access :=
                            UCD.Characters.Get (Mapping (J), CCC_Property);

                     begin
                        if J_CCC_Value = CCC_NR then
                           Found := True;

                           exit;
                        end if;
                     end;
                  end loop;

                  --  Implementation of the canonical composition algorithm
                  --  use some assumptions for optimization:
                  --
                  --   - if full decomposition mapping contains starters
                  --     then the first character of the full decomposition
                  --     mapping is starter (more starters may be present
                  --     inside the full decomposition mapping)
                  --
                  --   - such starter isn't last character of any pair of
                  --     the canonical decomposition of the any primary
                  --     composable character
                  --
                  --   Fail if some of these rules violated for some
                  --   character.

                  if Found then
                     declare
                        First_CCC_Value                               :
                        constant UCD.Properties.Property_Value_Access :=
                          UCD.Characters.Get
                            (Mapping.First_Element, CCC_Property);

                     begin
                        --   First character must be starter character.

                        if First_CCC_Value /= CCC_NR then
                           raise Program_Error;
                        end if;

                        --  and must not be last character of any pair of
                        --  canonical decomposition of the primary
                        --  composable character

                        if Is_Last_Code (Mapping.First_Element) then
                           raise Program_Error;
                        end if;
                     end;
                  end if;

               else
                  --  Check that there are no starters inside the decomposition

                  for J in reverse 1 .. Natural (Mapping.Length) loop
                     declare
                        CCC_Value : constant
                          UCD.Properties.Property_Value_Access :=
                            UCD.Characters.Get (Mapping (J), CCC_Property);

                     begin
                        if CCC_Value = CCC_NR then
                           raise Program_Error;
                        end if;
                     end;
                  end loop;
               end if;

               return Found;
            end Has_Starter;

            CCC_Value  : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, CCC_Property);
            DT_Value   : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, DT_Property);
            D_Mapping  : constant UCD.Code_Point_Vectors.Vector :=
              (if DT_Value = DT_Canonical
               then Full_Decomposition
                      (Code,
                       Canonical,
                       DT_Property, DT_None, DT_Canonical,
                       DM_Property)
               else UCD.Code_Point_Vectors.Empty_Vector);
            KD_Mapping : constant UCD.Code_Point_Vectors.Vector :=
              (if DT_Value /= DT_None
               then Full_Decomposition
                      (Code,
                       Compatibility,
                       DT_Property, DT_None, DT_Canonical,
                       DM_Property)
               else UCD.Code_Point_Vectors.Empty_Vector);

         begin
            --  Process information for canonical composition

            if DT_Value = DT_None then
               --  No decomposition

               Database.Set_NFC_Has_Starter (Code, CCC_Value = CCC_NR);
               Database.Set_NFKC_Has_Starter (Code, CCC_Value = CCC_NR);

            elsif DT_Value = DT_Canonical then
               --  Canonical decomposition

               Database.Set_NFC_Has_Starter
                 (Code, Has_Starter (CCC_Value, D_Mapping));
               Database.Set_NFKC_Has_Starter
                 (Code, Has_Starter (CCC_Value, D_Mapping));

            else
               --  All kinds of compatibility decomposition

               Database.Set_NFC_Has_Starter (Code, CCC_Value = CCC_NR);
               Database.Set_NFKC_Has_Starter
                 (Code, Has_Starter (CCC_Value, KD_Mapping));
            end if;
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
         Decomposition_QC : Boolean                 := True;
         Composition_QC   : Composition_Quick_Check := Yes;
         CCC              : Unsigned_6              := 0;
         Offset           : Unsigned_14             := 0;
         Size             : Unsigned_6              := 0;
         Length           : Unsigned_5              := 0;
         First_CCC        : Unsigned_6              := 0;
         Last_CCC         : Unsigned_6              := 0;
         First_Code_Index : Unsigned_9              := 0;
         Last_Code_Index  : Unsigned_6              := 0;
         Has_Starter      : Boolean                 := False;
         Reserved_1       : Unsigned_2              := 0;
      end record;
      for Mapping_Record'Size use 64;
      for Mapping_Record use record
         Offset           at 0 range 0 .. 13;
         Reserved_1       at 0 range 14 .. 15;
         First_Code_Index at 0 range 16 .. 24;
         Last_Code_Index  at 0 range 25 .. 30;
         Has_Starter      at 0 range 31 .. 31;
         Size             at 0 range 32 .. 37;
         CCC              at 0 range 38 .. 43;
         First_CCC        at 0 range 44 .. 49;
         Last_CCC         at 0 range 50 .. 55;
         Length           at 0 range 56 .. 60;
         Composition_QC   at 0 range 61 .. 62;
         Decomposition_QC at 0 range 63 .. 63;
      end record;
      --  This declaration must be synchronized with type declaration in the
      --  generated code.
      --
      --  This record contains additional information that may be derived or
      --  be copy of core properties when is in interest of normalization
      --  algorithms. This not increase total amount of the data for UCD, but
      --  allows to have all necessary data in one place, primary to minimize
      --  CPU cache usage.

      type Mapping_Array is array (UCD.Code_Point) of Mapping_Record;

      type Mapping_Array_Access is access all Mapping_Array;

      Raw_Mapping : array (Decomposition_Kind) of Mapping_Array_Access;

      First_Code_Count : Natural := 0;
      Last_Code_Count  : Natural := 0;

      type Composition_Mapping is
        array (Unsigned_6, Unsigned_9) of UCD.Code_Point;

      Raw_Composition_Mapping : Composition_Mapping :=
        (others => (others => 0));

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

      ------------------------------
      -- Composition_Data_Element --
      ------------------------------

      function Composition_Data_Element
        (First : Natural; Last : Natural) return UCD.Code_Point is
      begin
         return
           Raw_Composition_Mapping (Unsigned_6 (Last), Unsigned_9 (First));
      end Composition_Data_Element;

      ----------------------------------
      -- Composition_First_Index_Last --
      ----------------------------------

      function Composition_First_Index_Last return Natural is
      begin
         return First_Code_Count;
      end Composition_First_Index_Last;

      ---------------------------------
      -- Composition_Last_Index_Last --
      ---------------------------------

      function Composition_Last_Index_Last return Natural is
      begin
         return Last_Code_Count;
      end Composition_Last_Index_Last;

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

         Put_Line
           ("         first code points of mappings is"
            & Natural'Wide_Wide_Image (First_Code_Count)
            & ", last code points of mappings is"
            & Natural'Wide_Wide_Image (Last_Code_Count));
      end Print_Statistics;

      -------------------------
      -- Register_First_Code --
      -------------------------

      procedure Register_First_Code (Character : UCD.Code_Point) is
      begin
         First_Code_Count := First_Code_Count + 1;

         for Mapping in Decomposition_Kind loop
            Raw_Mapping (Mapping) (Character).First_Code_Index :=
              Unsigned_9 (First_Code_Count);
         end loop;
      end Register_First_Code;

      ------------------------
      -- Register_Last_Code --
      ------------------------

      procedure Register_Last_Code (Character : UCD.Code_Point) is
      begin
         Last_Code_Count := Last_Code_Count + 1;

         for Mapping in Decomposition_Kind loop
            Raw_Mapping (Mapping) (Character).Last_Code_Index :=
              Unsigned_6 (Last_Code_Count);
         end loop;
      end Register_Last_Code;

      ---------------------------------
      -- Set_Canonical_Decomposition --
      ---------------------------------

      procedure Set_Canonical_Decomposition
        (Character : UCD.Code_Point;
         Data      : UCD.Code_Point_Vectors.Vector)
      is
         First_CCC : constant Unsigned_6 :=
           Unsigned_6 (CCC_Enumeration.Representation (Data.First_Element));
         Last_CCC  : constant Unsigned_6 :=
           Unsigned_6 (CCC_Enumeration.Representation (Data.Last_Element));

         Offset    : UTF_8_Offset;
         Size      : UTF_8_Count;
         Length    : Natural;

      begin
         UTF_8_Data.Append_Data (Data, Offset, Size, Length);

         Raw_Mapping (Canonical) (Character).Offset    := Unsigned_14 (Offset);
         Raw_Mapping (Canonical) (Character).Size      := Unsigned_6 (Size);
         Raw_Mapping (Canonical) (Character).Length    := Unsigned_5 (Length);
         Raw_Mapping (Canonical) (Character).First_CCC := First_CCC;
         Raw_Mapping (Canonical) (Character).Last_CCC  := Last_CCC;

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
         Raw_Mapping (Canonical) (Character).CCC       := Unsigned_6 (To);
         Raw_Mapping (Canonical) (Character).First_CCC := Unsigned_6 (To);
         Raw_Mapping (Canonical) (Character).Last_CCC  := Unsigned_6 (To);
         Raw_Mapping (Compatibility) (Character).CCC       := Unsigned_6 (To);
         Raw_Mapping (Compatibility) (Character).Last_CCC  := Unsigned_6 (To);
         Raw_Mapping (Compatibility) (Character).First_CCC := Unsigned_6 (To);
      end Set_CCC;

      -------------------------------------
      -- Set_Compatibility_Decomposition --
      -------------------------------------

      procedure Set_Compatibility_Decomposition
        (Character : UCD.Code_Point;
         Data      : UCD.Code_Point_Vectors.Vector)
      is
         First_CCC : constant Unsigned_6 :=
           Unsigned_6 (CCC_Enumeration.Representation (Data.First_Element));
         Last_CCC  : constant Unsigned_6 :=
           Unsigned_6 (CCC_Enumeration.Representation (Data.Last_Element));

         Offset    : UTF_8_Offset;
         Size      : UTF_8_Count;
         Length    : Natural;

      begin
         UTF_8_Data.Append_Data (Data, Offset, Size, Length);

         Raw_Mapping (Compatibility) (Character).Offset    :=
           Unsigned_14 (Offset);
         Raw_Mapping (Compatibility) (Character).Size      :=
           Unsigned_6 (Size);
         Raw_Mapping (Compatibility) (Character).Length    :=
           Unsigned_5 (Length);
         Raw_Mapping (Compatibility) (Character).First_CCC := First_CCC;
         Raw_Mapping (Compatibility) (Character).Last_CCC  := Last_CCC;

         Max_Length  := Natural'Max (Max_Length, Length);
         Max_UTF_8   := Natural'Max (Max_UTF_8, Natural (Size));
         Total_UTF_8 := Total_UTF_8 + Natural (Size);
      end Set_Compatibility_Decomposition;

      -----------------------------
      -- Set_Composition_Mapping --
      -----------------------------

      procedure Set_Composition_Mapping
        (First   : UCD.Code_Point;
         Last    : UCD.Code_Point;
         Primary : UCD.Code_Point) is
      begin
         Raw_Composition_Mapping
           (Raw_Mapping (Canonical) (Last).Last_Code_Index,
            Raw_Mapping (Canonical) (First).First_Code_Index) := Primary;
      end Set_Composition_Mapping;

      -------------------------
      -- Set_NFC_Has_Starter --
      -------------------------

      procedure Set_NFC_Has_Starter
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         Raw_Mapping (Canonical) (Character).Has_Starter := To;
      end Set_NFC_Has_Starter;

      ----------------
      -- Set_NFC_QC --
      ----------------

      procedure Set_NFC_QC
        (Character : UCD.Code_Point;
         To        : Composition_Quick_Check) is
      begin
         Raw_Mapping (Canonical) (Character).Composition_QC := To;
      end Set_NFC_QC;

      ----------------
      -- Set_NFD_QC --
      ----------------

      procedure Set_NFD_QC
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         Raw_Mapping (Canonical) (Character).Decomposition_QC := To;
      end Set_NFD_QC;

      --------------------------
      -- Set_NFKC_Has_Starter --
      --------------------------

      procedure Set_NFKC_Has_Starter
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         Raw_Mapping (Compatibility) (Character).Has_Starter := To;
      end Set_NFKC_Has_Starter;

      -----------------
      -- Set_NFKC_QC --
      -----------------

      procedure Set_NFKC_QC
        (Character : UCD.Code_Point;
         To        : Composition_Quick_Check) is
      begin
         Raw_Mapping (Compatibility) (Character).Composition_QC := To;
      end Set_NFKC_QC;

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
      Put_Line (File, "with VSS.Implementation.UCD_Normalization_Common;");
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
         "   type Composition_Quick_Check is (No, Maybe, Yes);");
      New_Line (File);

      Put_Line
        (File,
         "   type Mapping_Information is record");
      Put_Line (File, "      Decomposition_QC : Boolean;");
      Put_Line (File, "      Composition_QC   : Composition_Quick_Check;");
      Put_Line (File, "      CCC              : CCC_Values;");
      Put_Line
        (File, "      Offset           : Normalization_UTF8_Data_Offset;");
      Put_Line
        (File, "      Size             : Normalization_UTF8_Code_Unit_Count;");
      Put_Line
        (File, "      Length           : Normalization_Character_Count;");
      Put_Line (File, "      First_CCC        : CCC_Values;");
      Put_Line (File, "      Last_CCC         : CCC_Values;");
      Put_Line (File, "      First_Index      :");
      Put_Line
        (File,
         "        VSS.Implementation.UCD_Normalization_Common"
         & ".First_Mapping_Code_Offset;");
      Put_Line (File, "      Last_Index       :");
      Put_Line
        (File,
         "        VSS.Implementation.UCD_Normalization_Common"
         & ".Last_Mapping_Code_Offset;");
      Put_Line (File, "      Has_Starter      : Boolean;");
      Put_Line (File, "   end record;");
      Put_Line
        (File,
         "   for Mapping_Information'Size use 64;");
      Put_Line (File, "   for Mapping_Information use record");
      Put_Line (File, "      Offset           at 0 range 0 .. 13;");
      Put_Line (File, "      First_Index      at 0 range 16 .. 24;");
      Put_Line (File, "      Last_Index       at 0 range 25 .. 30;");
      Put_Line (File, "      Has_Starter      at 0 range 31 .. 31;");
      Put_Line (File, "      Size             at 0 range 32 .. 37;");
      Put_Line (File, "      CCC              at 0 range 38 .. 43;");
      Put_Line (File, "      First_CCC        at 0 range 44 .. 49;");
      Put_Line (File, "      Last_CCC         at 0 range 50 .. 55;");
      Put_Line (File, "      Length           at 0 range 56 .. 60;");
      Put_Line (File, "      Composition_QC   at 0 range 61 .. 62;");
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

      --  Generate common composition data

      Put_Line (File, "pragma Restrictions (No_Elaboration_Code);");
      New_Line (File);

      Put_Line (File, "with VSS.Unicode;");
      New_Line (File);

      Put_Line
        (File, "package VSS.Implementation.UCD_Normalization_Common is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      New_Line (File);

      Put_Line
        (File,
         "   type First_Mapping_Code_Offset is range 0 .."
         & Natural'Wide_Wide_Image (Database.Composition_First_Index_Last)
         & ';');
      Put_Line
        (File,
         "   type Last_Mapping_Code_Offset is range 0 .."
         & Natural'Wide_Wide_Image (Database.Composition_Last_Index_Last)
         & ';');
      New_Line (File);

      Put_Line (File, "   Composition_Mapping :");
      Put_Line
        (File,
         "     constant array (Last_Mapping_Code_Offset,"
         & " First_Mapping_Code_Offset)");
      Put_Line (File, "       of VSS.Unicode.Code_Point :=");
      Put (File, "        (");

      for L in 0 .. Database.Composition_Last_Index_Last loop
         if L /= 0 then
            Put (File, ",");
            New_Line (File);
            Put (File, "         ");
         end if;

         Put (File, "(");

         for F in 0 .. Database.Composition_First_Index_Last loop
            declare
               package Code_Point_IO is
                 new Ada.Wide_Wide_Text_IO.Integer_IO (UCD.Code_Point);
               use Code_Point_IO;

               Image : Wide_Wide_String (1 .. 11);

            begin
               Put (Image, Database.Composition_Data_Element (F, L), 16);

               if F = 0 then
                  null;

               elsif F mod 6 = 0 then
                  Put_Line (File, ",");
                  Put (File, "          ");

               else
                  Put (File, ", ");
               end if;

               Put (File, Trim (Image, Both));
            end;
         end loop;

         Put (File, ")");
      end loop;

      Put_Line (File, ");");
      New_Line (File);

      Put_Line (File, "end VSS.Implementation.UCD_Normalization_Common;");
   end Generate;

end Gen_UCD.Normalization;
