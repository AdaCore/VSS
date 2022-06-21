--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Integer_Wide_Wide_Text_IO;     use Ada.Integer_Wide_Wide_Text_IO;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;       use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;

with UCD.Characters;
with UCD.Properties;

with Gen_UCD.Compressed_UTF_8_Data;
with Gen_UCD.Generic_Compressed_Stage_Table;

package body Gen_UCD.Casing is

   use type UCD.Code_Point;

   package Database is

      type Case_Mapping is
        (Simple_Lowercase,
         Simple_Titlecase,
         Simple_Uppercase,
         Simple_Case_Folding,
         NFKC_Casefold,
         Full_Lowercase,
         Full_Titlecase,
         Full_Uppercase,
         Full_Case_Folding);

      subtype Simple_Case_Mapping is Case_Mapping
        range Simple_Lowercase .. NFKC_Casefold;

      subtype Full_Case_Mapping is Case_Mapping
        range Full_Lowercase .. Full_Case_Folding;

      procedure Initialize;

      procedure Set
        (Character : UCD.Code_Point;
         Mapping   : Case_Mapping;
         Data      : UCD.Code_Point_Vectors.Vector);

      procedure Set_Cased
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_Case_Ignorable
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_Final_Sigma_Enter
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_Final_Sigma_Continue
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_After_Soft_Dotted_Enter
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_After_Soft_Dotted_Continue
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_After_I_Enter
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Set_After_I_Continue
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Compress;

      function UTF_8_Data_Index_Last return Gen_UCD.UTF_8_Offset;

      function UTF_8_Data_Element
        (Index : Gen_UCD.UTF_8_Offset) return Gen_UCD.UTF_8_Code_Unit;

      function Mapping_Data_Last return Natural;

      function Mapping_Data_Element (Offset : Natural) return Unsigned_32;

      function Mapping_Index_Last (Mapping : Case_Mapping) return Natural;

      function Mapping_Index_Element
        (Mapping : Case_Mapping;
         Offset  : Natural) return Natural;

      function Mapping_Index_Group_Size
        (Mapping : Case_Mapping) return Natural;

      procedure Print_Statistics;

   end Database;

   -----------
   -- Build --
   -----------

   procedure Build is
      SUC_Property     : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("suc");
      SLC_Property     : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("slc");
      STC_Property     : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("stc");
      SCF_Property     : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("scf");
      LC_Property      : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("lc");
      TC_Property      : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("tc");
      UC_Property      : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("uc");
      CF_Property      : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("cf");
      NFKC_CF_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("NFKC_CF");

      Cased_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("Cased");
      Cased_Y        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (Cased_Property, "Y");
      CI_Property    :  constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("CI");
      CI_Y           : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (CI_Property, "Y");
      SD_Property    : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("SD");
      SD_Y           : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (SD_Property, "Y");
      CCC_Property   : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("ccc");
      CCC_0          : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (CCC_Property, "Not_Reordered");
      CCC_230        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (CCC_Property, "Above");

   begin
      Put_Line ("   ... casing");

      Database.Initialize;

      --  Process properties of each character. Do it in reverse order, it
      --  produce little bit (around 40 bytes) smaller table.

      for Code in reverse UCD.Code_Point loop
         declare
            use type UCD.Properties.Property_Value_Access;

            SUC_Value : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, SUC_Property);
            SLC_Value : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, SLC_Property);
            STC_Value : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, STC_Property);
            SCF_Value : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, SCF_Property);

            LC_Value  : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, LC_Property);
            TC_Value  : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, TC_Property);
            UC_Value  : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, UC_Property);
            CF_Value  : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, CF_Property);

            NFKC_CF_Value : constant UCD.Properties.Property_Value_Access :=
              UCD.Characters.Get (Code, NFKC_CF_Property);

            Cased_Value   : constant Boolean :=
              UCD.Characters.Get (Code, Cased_Property) = Cased_Y;
            CI_Value      : constant Boolean :=
              UCD.Characters.Get (Code, CI_Property) = CI_Y;
            SD_Value      : constant Boolean :=
              UCD.Characters.Get (Code, SD_Property) = SD_Y;
            CCC_0_Value   : constant Boolean :=
              UCD.Characters.Get (Code, CCC_Property) = CCC_0;
            CCC_230_Value : constant Boolean :=
              UCD.Characters.Get (Code, CCC_Property) = CCC_230;

         begin
            if SUC_Value /= null then
               Database.Set
                 (Code, Database.Simple_Uppercase, SUC_Value.String);
            end if;

            if SLC_Value /= null then
               Database.Set
                 (Code, Database.Simple_Lowercase, SLC_Value.String);
            end if;

            if STC_Value /= null then
               Database.Set
                 (Code, Database.Simple_Titlecase, STC_Value.String);
            end if;

            if SCF_Value /= null then
               Database.Set
                 (Code, Database.Simple_Case_Folding, SCF_Value.String);
            end if;

            if LC_Value /= null then
               Database.Set (Code, Database.Full_Lowercase, LC_Value.String);

            elsif SLC_Value /= null then
               Database.Set
                 (Code, Database.Full_Lowercase, SLC_Value.String);
            end if;

            if TC_Value /= null then
               Database.Set (Code, Database.Full_Titlecase, TC_Value.String);

            elsif STC_Value /= null then
               Database.Set
                 (Code, Database.Full_Titlecase, STC_Value.String);
            end if;

            if UC_Value /= null then
               Database.Set (Code, Database.Full_Uppercase, UC_Value.String);

            elsif SUC_Value /= null then
               Database.Set
                 (Code, Database.Full_Uppercase, SUC_Value.String);
            end if;

            if CF_Value /= null then
               Database.Set
                 (Code, Database.Full_Case_Folding, CF_Value.String);

            elsif SCF_Value /= null then
               Database.Set
                 (Code, Database.Full_Case_Folding, SCF_Value.String);
            end if;

            if NFKC_CF_Value /= null then
               Database.Set
                 (Code, Database.NFKC_Casefold, NFKC_CF_Value.String);
            end if;

            Database.Set_Cased (Code, Cased_Value);
            Database.Set_Case_Ignorable (Code, CI_Value);

            --  Compute changes for casing context.

            Database.Set_Final_Sigma_Enter
              (Code, Cased_Value and not CI_Value);
            Database.Set_Final_Sigma_Continue (Code, CI_Value);
            Database.Set_After_Soft_Dotted_Enter (Code, SD_Value);
            Database.Set_After_Soft_Dotted_Continue
              (Code, not (CCC_0_Value or CCC_230_Value));
            Database.Set_After_I_Enter
              (Code, Code = Wide_Wide_Character'Pos ('I'));
            Database.Set_After_I_Continue
              (Code, not (CCC_0_Value or CCC_230_Value));
         end;
      end loop;

      Database.Compress;

      Database.Print_Statistics;
   end Build;

   --------------
   -- Database --
   --------------

   package body Database is

      type Casing_Context_Change is record
         Enter_Final_Sigma          : Boolean := False;
         Continue_Final_Sigma       : Boolean := False;
         Enter_After_Soft_Dotted    : Boolean := False;
         Continue_After_Soft_Dotted : Boolean := False;
         Enter_After_I              : Boolean := False;
         Continue_After_I           : Boolean := False;
      end record with Pack;
      for Casing_Context_Change'Size use 6;
      --  This type must be synchronized with type in the package
      --  VSS.Implementation.UCD_Casing.

      type Simplified_Mapping_Record is record
         Offset     : Unsigned_15 := 0;
         Length     : Unsigned_5  := 0;
         Size       : Unsigned_6  := 0;
         Changes    : Boolean     := False;
         Reserved_1 : Unsigned_1  := 0;
         Reserved_2 : Unsigned_3  := 0;
         Reserved_3 : Unsigned_1  := 0;
      end record;
      for Simplified_Mapping_Record'Size use 32;
      for Simplified_Mapping_Record use record
         Offset     at 0 range 0 .. 14;
         Reserved_1 at 0 range 15 .. 15;
         Length     at 0 range 16 .. 20;
         Reserved_2 at 0 range 21 .. 23;
         Size       at 0 range 24 .. 29;
         Reserved_3 at 0 range 30 .. 30;
         Changes    at 0 range 31 .. 31;
      end record;
      --  This declaration must be synchronized with type declaration in the
      --  generated code.
      --
      --  This record contains additional information that may be derived or
      --  by copy of core properties when is in interest of casing algorithms.
      --  This not increase total amount of the data for UCD, but allows to
      --  have all necessary data in one place, primary to minimize CPU cache
      --  usage.

      type Contextual_Mapping_Record is record
         Offset         : Unsigned_15 := 0;
         Length         : Unsigned_2  := 0;
         Size           : Unsigned_3  := 0;
         Context_Change : Casing_Context_Change;
         Cased          : Boolean     := False;
         Case_Ignorable : Boolean     := False;
         Changes        : Boolean     := False;
         Reserved_1     : Unsigned_1  := 0;
         Reserved_2     : Unsigned_1  := 0;
         Reserved_3     : Unsigned_1  := 0;
      end record;
      for Contextual_Mapping_Record'Size use 32;
      for Contextual_Mapping_Record use record
         Offset         at 0 range 0 .. 14;
         Reserved_1     at 0 range 15 .. 15;
         Length         at 0 range 16 .. 17;
         Context_Change at 0 range 18 .. 23;
         Size           at 0 range 24 .. 26;
         Reserved_2     at 0 range 27 .. 27;
         Cased          at 0 range 28 .. 28;
         Case_Ignorable at 0 range 29 .. 29;
         Reserved_3     at 0 range 30 .. 30;
         Changes        at 0 range 31 .. 31;
      end record;
      --  This declaration must be synchronized with type declaration in the
      --  generated code.
      --
      --  This record contains additional information that may be derived or
      --  by copy of core properties when is in interest of casing algorithms.
      --  This not increase total amount of the data for UCD, but allows to
      --  have all necessary data in one place, primary to minimize CPU cache
      --  usage.

      type Mapping_Record (Is_Simplified : Boolean := False) is record
         case Is_Simplified is
            when False =>
               Contextual : Contextual_Mapping_Record;

            when True =>
               Simplified : Simplified_Mapping_Record;
         end case;
      end record with Unchecked_Union, Size => 32;

      overriding function "="
        (Left : Mapping_Record; Right : Mapping_Record) return Boolean;

      type Mapping_Array is array (UCD.Code_Point) of Mapping_Record;

      type Mapping_Array_Access is access all Mapping_Array;

      Raw_Mapping : array (Case_Mapping) of Mapping_Array_Access;

      Max_Length  : Natural := 0;
      Max_UTF_8   : Natural := 0;
      Total_UTF_8 : Natural := 0;

      package Compressed_Stage_Table is
        new Gen_UCD.Generic_Compressed_Stage_Table
              (Mapping_Record, Mapping_Array);

      Compressed :
        array (Case_Mapping) of Compressed_Stage_Table.Compressed_Stage_Table;

      UTF_8_Data : Gen_UCD.Compressed_UTF_8_Data.Compressed_UTF_8_Data;

      ---------
      -- "=" --
      ---------

      overriding function "="
        (Left : Mapping_Record; Right : Mapping_Record) return Boolean
      is
         function To_Unsigned_32 is
           new Ada.Unchecked_Conversion (Mapping_Record, Unsigned_32);

      begin
         return To_Unsigned_32 (Left) = To_Unsigned_32 (Right);
      end "=";

      --------------
      -- Compress --
      --------------

      procedure Compress is
      begin
         for J in Case_Mapping'Range loop
            Compressed (J).Build (Raw_Mapping (J).all);
         end loop;
      end Compress;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         for M in Case_Mapping'Range loop
            Raw_Mapping (M) := new Mapping_Array;
         end loop;
      end Initialize;

      --------------------------
      -- Mapping_Data_Element --
      --------------------------

      function Mapping_Data_Element (Offset : Natural) return Unsigned_32 is
         function To_Unsigned_32 is
            new Ada.Unchecked_Conversion (Mapping_Record, Unsigned_32);

      begin
         return
           To_Unsigned_32
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
        (Mapping : Case_Mapping;
         Offset  : Natural) return Natural is
      begin
         return
           Natural
             (Compressed (Mapping).Index_Table_Element
              (Compressed_Stage_Table.Group_Offset (Offset)));
      end Mapping_Index_Element;

      ------------------------------
      -- Mapping_Index_Group_Size --
      ------------------------------

      function Mapping_Index_Group_Size
        (Mapping : Case_Mapping) return Natural
      is
         pragma Unreferenced (Mapping);

      begin
         return Compressed_Stage_Table.Group_Size;
      end Mapping_Index_Group_Size;

      ------------------------
      -- Mapping_Index_Last --
      ------------------------

      function Mapping_Index_Last (Mapping : Case_Mapping) return Natural is
      begin
         return Natural (Compressed (Mapping).Index_Table_Last);
      end Mapping_Index_Last;

      ----------------------
      -- Print_Statistics --
      ----------------------

      procedure Print_Statistics is
      begin
         Put_Line
           ("         max expanded length is"
            & Natural'Wide_Wide_Image (Max_Length)
            & " max UTF-8 sequence is"
            & Natural'Wide_Wide_Image (Max_UTF_8));

         Put_Line
           ("         UTF-8 data size is"
            & Natural'Wide_Wide_Image (Total_UTF_8)
            & " bytes (compressed size is"
            & Natural'Wide_Wide_Image (Natural (UTF_8_Data.Last_Index) + 1)
            & " bytes)");
      end Print_Statistics;

      ---------
      -- Set --
      ---------

      procedure Set
        (Character : UCD.Code_Point;
         Mapping   : Case_Mapping;
         Data      : UCD.Code_Point_Vectors.Vector)
      is
         Offset : Gen_UCD.UTF_8_Offset;
         Size   : Gen_UCD.UTF_8_Count;
         Length : Natural;

      begin
         UTF_8_Data.Append_Data (Data, Offset, Size, Length);

         if Mapping in Simple_Case_Mapping then
            Raw_Mapping (Mapping) (Character).Simplified.Changes := True;
            Raw_Mapping (Mapping) (Character).Simplified.Offset  :=
              Unsigned_15 (Offset);
            Raw_Mapping (Mapping) (Character).Simplified.Size    :=
              Unsigned_6 (Size);
            Raw_Mapping (Mapping) (Character).Simplified.Length  :=
              Unsigned_5 (Length);

         else
            Raw_Mapping (Mapping) (Character).Contextual.Changes := True;
            Raw_Mapping (Mapping) (Character).Contextual.Offset  :=
              Unsigned_15 (Offset);
            Raw_Mapping (Mapping) (Character).Contextual.Size    :=
              Unsigned_3 (Size);
            Raw_Mapping (Mapping) (Character).Contextual.Length  :=
              Unsigned_2 (Length);
         end if;

         Max_Length  := Natural'Max (Max_Length, Length);
         Max_UTF_8   := Natural'Max (Max_UTF_8, Natural (Size));
         Total_UTF_8 := Total_UTF_8 + Natural (Size);
      end Set;

      --------------------------
      -- Set_After_I_Continue --
      --------------------------

      procedure Set_After_I_Continue
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Context_Change
              .Continue_After_I := To;
         end loop;
      end Set_After_I_Continue;

      -----------------------
      -- Set_After_I_Enter --
      -----------------------

      procedure Set_After_I_Enter
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Context_Change
              .Enter_After_I := To;
         end loop;
      end Set_After_I_Enter;

      ------------------------------------
      -- Set_After_Soft_Dotted_Continue --
      ------------------------------------

      procedure Set_After_Soft_Dotted_Continue
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Context_Change
              .Continue_After_Soft_Dotted := To;
         end loop;
      end Set_After_Soft_Dotted_Continue;

      ---------------------------------
      -- Set_After_Soft_Dotted_Enter --
      ---------------------------------

      procedure Set_After_Soft_Dotted_Enter
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Context_Change
              .Enter_After_Soft_Dotted := To;
         end loop;
      end Set_After_Soft_Dotted_Enter;

      ------------------------
      -- Set_Case_Ignorable --
      ------------------------

      procedure Set_Case_Ignorable
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Case_Ignorable := To;
         end loop;
      end Set_Case_Ignorable;

      ---------------
      -- Set_Cased --
      ---------------

      procedure Set_Cased
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Cased := To;
         end loop;
      end Set_Cased;

      ------------------------------
      -- Set_Final_Sigma_Continue --
      ------------------------------

      procedure Set_Final_Sigma_Continue
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Context_Change
              .Continue_Final_Sigma := To;
         end loop;
      end Set_Final_Sigma_Continue;

      ---------------------------
      -- Set_Final_Sigma_Enter --
      ---------------------------

      procedure Set_Final_Sigma_Enter
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Full_Case_Mapping loop
            Raw_Mapping (Mapping) (Character).Contextual.Context_Change
              .Enter_Final_Sigma := To;
         end loop;
      end Set_Final_Sigma_Enter;

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

   --------------
   -- Generate --
   --------------

   procedure Generate (File : Ada.Wide_Wide_Text_IO.File_Type) is

      procedure Generate_Index_Table
        (Mapping : Database.Case_Mapping;
         Name    : Wide_Wide_String);

      --------------------------
      -- Generate_Index_Table --
      --------------------------

      procedure Generate_Index_Table
        (Mapping : Database.Case_Mapping;
         Name    : Wide_Wide_String)
      is
         Image : Wide_Wide_String (1 .. 8);

      begin
         Put_Line (File, "   " & Name & " :");
         Put_Line (File, "     constant Mapping_Data_Offset_Array :=");

         for J in 0 .. Database.Mapping_Index_Last (Mapping) loop
            Put (Image, Database.Mapping_Index_Element (Mapping, J));

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
      Put_Line ("   ... casing");

      Put_Line (File, "pragma Restrictions (No_Elaboration_Code);");
      New_Line (File);

      Put_Line (File, "with VSS.Implementation.Strings;");
      Put_Line (File, "with VSS.Implementation.UCD_Casing;");
      Put_Line (File, "with VSS.Implementation.UTF8_Encoding;");
      Put_Line (File, "with VSS.Unicode;");
      Put_Line (File, "with Interfaces;");
      New_Line (File);
      Put_Line (File, "package VSS.Implementation.UCD_Casing_UTF8 is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      New_Line (File);

      --  Generate data types

      Put_Line
        (File,
         "   subtype Casing_UTF8_Data_Offset is");
      Put_Line
        (File,
         "     VSS.Unicode.UTF8_Code_Unit_Offset range 0 .."
         & Positive'Wide_Wide_Image (Positive (Database.UTF_8_Data_Index_Last))
         & ";");
      New_Line (File);

      Put_Line
        (File,
         "   subtype Simplified_Casing_Character_Count is");
      Put_Line
        (File,
         "     VSS.Implementation.Strings.Character_Count range 0 .. 18;");
      New_Line (File);

      Put_Line
        (File,
         "   subtype Contextual_Casing_Character_Count is");
      Put_Line
        (File,
         "     VSS.Implementation.Strings.Character_Count range 0 .. 3;");
      New_Line (File);

      Put_Line
        (File,
         "   subtype Simplified_Casing_UTF8_Code_Unit_Count is");
      Put_Line
        (File,
         "     VSS.Unicode.UTF8_Code_Unit_Count range 0 .. 33;");
      New_Line (File);
      Put_Line
        (File,
         "   subtype Contextual_Casing_UTF8_Code_Unit_Count is");
      Put_Line
        (File,
         "     VSS.Unicode.UTF8_Code_Unit_Count range 0 .. 6;");
      New_Line (File);

      Put_Line
        (File,
         "   type Simplified_Mapping_Information is record");
      Put_Line (File, "      Offset  : Casing_UTF8_Data_Offset;");
      Put_Line (File, "      Length  : Simplified_Casing_Character_Count;");
      Put_Line
        (File, "      Count   : Simplified_Casing_UTF8_Code_Unit_Count;");
      Put_Line (File, "      Changes : Boolean;");
      Put_Line
        (File,
         "      --  Equivalent of Changes_On_<mapping> for the given mapping");
      Put_Line (File, "   end record;");
      Put_Line
        (File,
         "   for Simplified_Mapping_Information'Size use 32;");
      Put_Line (File, "   for Simplified_Mapping_Information use record");
      Put_Line (File, "      Offset  at 0 range 0 .. 14;");
      Put_Line (File, "      Length  at 0 range 16 .. 20;");
      Put_Line (File, "      Count   at 0 range 24 .. 29;");
      Put_Line (File, "      Changes at 0 range 31 .. 31;");
      Put_Line (File, "   end record;");
      New_Line (File);

      Put_Line
        (File,
         "   type Contextual_Mapping_Information is record");
      Put_Line (File, "      Offset         : Casing_UTF8_Data_Offset;");
      Put_Line
        (File, "      Length         : Contextual_Casing_Character_Count;");
      Put_Line
        (File,
         "      Count          : Contextual_Casing_UTF8_Code_Unit_Count;");
      Put_Line
        (File,
         "      Context_Change :"
         & " VSS.Implementation.UCD_Casing.Casing_Context_Change;");
      Put_Line (File, "      Cased          : Boolean;");
      Put_Line (File, "      Case_Ignorable : Boolean;");
      Put_Line (File, "      Changes        : Boolean;");
      Put_Line
        (File,
         "      --  Equivalent of Changes_On_<mapping> for the given mapping");
      Put_Line (File, "   end record;");
      Put_Line
        (File,
         "   for Contextual_Mapping_Information'Size use 32;");
      Put_Line (File, "   for Contextual_Mapping_Information use record");
      Put_Line (File, "      Offset         at 0 range 0 .. 14;");
      Put_Line (File, "      Length         at 0 range 16 .. 17;");
      Put_Line (File, "      Context_Change at 0 range 18 .. 23;");
      Put_Line (File, "      Count          at 0 range 24 .. 26;");
      Put_Line (File, "      Cased          at 0 range 28 .. 28;");
      Put_Line (File, "      Case_Ignorable at 0 range 29 .. 29;");
      Put_Line (File, "      Changes        at 0 range 31 .. 31;");
      Put_Line (File, "   end record;");
      New_Line (File);

      Put_Line
        (File,
         "   Mapping_Group_Size : constant :="
         & Integer'Wide_Wide_Image
           (Database.Mapping_Index_Group_Size (Database.Simple_Lowercase))
         & ";");
      New_Line (File);

      Put_Line
        (File,
         "   type Mapping_Group is range 0 .."
         & Integer'Wide_Wide_Image
           (Database.Mapping_Index_Last (Database.Simple_Lowercase))
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

      --  Generate case mappings indices.

      Generate_Index_Table
        (Database.Simple_Lowercase, "Simple_Lowercase_Index");
      Generate_Index_Table
        (Database.Simple_Titlecase, "Simple_Titlecase_Index");
      Generate_Index_Table
        (Database.Simple_Uppercase, "Simple_Uppercase_Index");
      Generate_Index_Table
        (Database.Simple_Case_Folding, "Simple_Case_Folding_Index");

      Generate_Index_Table
        (Database.Full_Lowercase, "Full_Lowercase_Index");
      Generate_Index_Table
        (Database.Full_Titlecase, "Full_Titlecase_Index");
      Generate_Index_Table
        (Database.Full_Uppercase, "Full_Uppercase_Index");
      Generate_Index_Table
        (Database.Full_Case_Folding, "Full_Case_Folding_Index");

      Generate_Index_Table (Database.NFKC_Casefold, "NFKC_Casefold_Index");

      --  Generate mapping data table.

      declare

         package Mapping_Data_IO is
           new Ada.Wide_Wide_Text_IO.Modular_IO (Unsigned_32);
         use Mapping_Data_IO;

         Image : Wide_Wide_String (1 .. 12);

      begin
         Put_Line
           (File,
            "   Mapping_Data_Table_Raw :");
         Put_Line
           (File,
            "     constant array (Mapping_Data_Offset)");
         Put_Line
           (File,
            "       of Interfaces.Unsigned_32 :=");

         for J in 0 .. Database.Mapping_Data_Last loop
            Put (Image, Database.Mapping_Data_Element (J), 16);

            if J = 0 then
               Put (File, "         (");

            elsif J mod 4 = 0 then
               Put_Line (File, ",");
               Put (File, "          ");

            else
               Put (File, ", ");
            end if;

            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, ");");
         New_Line (File);

         Put_Line (File, "   Simplified_Mapping_Data_Table :");
         Put_Line
           (File,
            "     constant array (Mapping_Data_Offset)"
            & " of Simplified_Mapping_Information");
         Put_Line (File, "       with Import,");
         Put_Line (File, "            Convention => Ada,");
         Put_Line
           (File,
            "            Address    => Mapping_Data_Table_Raw'Address;");
         New_Line (File);

         Put_Line (File, "   Contextual_Mapping_Data_Table :");
         Put_Line
           (File,
            "     constant array (Mapping_Data_Offset)"
            & " of Contextual_Mapping_Information");
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
            "       (Casing_UTF8_Data_Offset) :=");

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

      Put_Line (File, "end VSS.Implementation.UCD_Casing_UTF8;");
   end Generate;

end Gen_UCD.Casing;
