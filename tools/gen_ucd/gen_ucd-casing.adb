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

with Ada.Containers.Vectors;
with Ada.Integer_Wide_Wide_Text_IO;     use Ada.Integer_Wide_Wide_Text_IO;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;       use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;
with Interfaces;

with UCD.Characters;
with UCD.Properties;

with Gen_UCD.Unsigned_Types;            use Gen_UCD.Unsigned_Types;

package body Gen_UCD.Casing is

   use type UCD.Code_Point;

   generic
      type Data_Type is private;
      type Data_Type_Array is array (UCD.Code_Point) of Data_Type;

   package Generic_Compressed_Stage_Table is

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

      type Group_Array is array (Unsigned_32 range <>) of Unsigned_32;

      type Group_Array_Access is access all Group_Array;

      type Compressed_Stage_Table is tagged limited record
         Group_Data : Group_Array_Access;
      end record;

   end Generic_Compressed_Stage_Table;

   ------------------------------------
   -- Generic_Compressed_Stage_Table --
   ------------------------------------

   package body Generic_Compressed_Stage_Table is

      type Compressed_Array is array (Unsigned_32 range <>) of Data_Type;

      type Compressed_Array_Access is access all Compressed_Array;

      Result_Data       : Compressed_Array_Access;
      Result_Data_Last  : Unsigned_32;

      -----------
      -- Build --
      -----------

      procedure Build
        (Self : in out Compressed_Stage_Table'Class; Data : Data_Type_Array)
      is
         Initial : Unsigned_32;
         Reused  : Boolean;

      begin
         Self.Group_Data :=
           new Group_Array
             (0 .. (Unsigned_32 (UCD.Code_Point'Last) + 1) / Group_Size - 1);

         if Result_Data = null then
            --  Allocate memory.

            Result_Data :=
              new Compressed_Array (0 .. Unsigned_32 (UCD.Code_Point'Last));

            --  Copy first block

            Result_Data (0 .. Group_Size - 1) :=
              Compressed_Array (Data (0 .. Group_Size - 1));
            Result_Data_Last := Group_Size - 1;
            Self.Group_Data (0) := 0;
            Initial := 1;

         else
            Initial := 0;
         end if;

         --  Process all other blocks

         for Group in Initial .. Self.Group_Data'Last loop
            declare
               Source : Compressed_Array renames
                 Compressed_Array
                     (Data (UCD.Code_Point (Group * Group_Size)
                      .. UCD.Code_Point ((Group + 1) * Group_Size - 1)));

            begin
               Reused := False;

               for Offset in 0 .. Result_Data_Last - Group_Size + 1 loop
                  if Result_Data (Offset .. Offset + Group_Size - 1)
                    = Source
                  then
                     Self.Group_Data (Group) := Offset;
                     Reused := True;

                     exit;
                  end if;
               end loop;

               if not Reused then
                  Self.Group_Data (Group) := Result_Data_Last + 1;
                  Result_Data_Last := Result_Data_Last + Group_Size;
                  Result_Data
                    (Result_Data_Last - Group_Size + 1 .. Result_Data_Last) :=
                       Source;
               end if;
            end;
         end loop;
      end Build;

      ------------------------
      -- Data_Table_Element --
      ------------------------

      function Data_Table_Element (Offset : Data_Offset) return Data_Type is
      begin
         return Result_Data (Unsigned_32 (Offset));
      end Data_Table_Element;

      ---------------------
      -- Data_Table_Last --
      ---------------------

      function Data_Table_Last return Data_Count is
      begin
         return Data_Count (Result_Data_Last);
      end Data_Table_Last;

      -------------------------
      -- Index_Table_Element --
      -------------------------

      function Index_Table_Element
        (Self   : Compressed_Stage_Table'Class;
         Offset : Group_Offset) return Data_Offset is
      begin
         return Data_Offset (Self.Group_Data (Unsigned_32 (Offset)));
      end Index_Table_Element;

      ----------------------
      -- Index_Table_Last --
      ----------------------

      function Index_Table_Last
        (Self : Compressed_Stage_Table'Class) return Group_Count is
      begin
         return Group_Count (Self.Group_Data'Last);
      end Index_Table_Last;

   end Generic_Compressed_Stage_Table;

   package Database is

      type Case_Mapping is
        (Simple_Lowercase,
         Simple_Titlecase,
         Simple_Uppercase,
         Simple_Case_Folding,
         Full_Lowercase,
         Full_Titlecase,
         Full_Uppercase,
         Full_Case_Folding);

      procedure Initialize;

      procedure Set
        (Character : UCD.Code_Point;
         Mapping   : Case_Mapping;
         Data      : UCD.Code_Point_Vectors.Vector);

      procedure Set_NFD_QC
        (Character : UCD.Code_Point;
         To        : Boolean);

      procedure Compress;

      function UTF_8_Data_Index_Last return Natural;

      function UTF_8_Data_Element (Index : Natural) return Unsigned_8;

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
      SUC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("suc");
      SLC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("slc");
      STC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("stc");
      SCF_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("scf");
      LC_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("lc");
      TC_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("tc");
      UC_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("uc");
      CF_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("cf");

      NFD_QC_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("NFD_QC");
      NFD_QC_Y        : constant not null
        UCD.Properties.Property_Value_Access :=
          UCD.Properties.Resolve (NFD_QC_Property, "Y");

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

            NFD_QC_Value : constant Boolean :=
              UCD.Characters.Get (Code, NFD_QC_Property) = NFD_QC_Y;

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

            Database.Set_NFD_QC (Code, NFD_QC_Value);
         end;
      end loop;

      Database.Compress;

      Database.Print_Statistics;
   end Build;

   --------------
   -- Database --
   --------------

   package body Database is

      type UTF_8_Code_Unit is mod 2 ** 8;
      for UTF_8_Code_Unit'Size use 8;

      package UTF_8_Code_Unit_Vectors is
        new Ada.Containers.Vectors (Positive, UTF_8_Code_Unit);

      UTF_8_Data      : UTF_8_Code_Unit_Vectors.Vector;
      UTF_8_Data_Size : Natural := 0;

      type Mapping_Record is record
         Offset      : Unsigned_14 := 0;
         Length      : Unsigned_2  := 0;
         Size        : Unsigned_3  := 0;
         Has_Mapping : Boolean     := False;
         NFD_QC      : Boolean     := False;
         Reserved_1  : Unsigned_2  := 0;
         Reserved_2  : Unsigned_6  := 0;
         Reserved_3  : Unsigned_3  := 0;
      end record;
      for Mapping_Record'Size use 32;
      for Mapping_Record use record
         Offset      at 0 range 0 .. 13;
         Reserved_1  at 0 range 14 .. 15;
         Length      at 0 range 16 .. 17;
         Reserved_2  at 0 range 18 .. 23;
         Size        at 0 range 24 .. 26;
         Reserved_3  at 0 range 27 .. 29;
         NFD_QC      at 0 range 30 .. 30;
         Has_Mapping at 0 range 31 .. 31;
      end record;
      --  This declaration must be synchronized with type declaration in the
      --  generated code.
      --
      --  This record contains additional information that may be derived or
      --  by copy of core properties when is in interest of casing algoriphms.
      --  This not increase total amount of the data for UCD, but allows to
      --  have all necessary data in one place, primary to mininize CPU cache
      --  usage.

      type Mapping_Array is array (UCD.Code_Point) of Mapping_Record;

      type Mapping_Array_Access is access all Mapping_Array;

      Raw_Mapping : array (Case_Mapping) of Mapping_Array_Access;

      Max_Length : Natural := 0;
      Max_UTF_8  : Natural := 0;

      function Append_Data
        (Data : UCD.Code_Point_Vectors.Vector) return Mapping_Record;

      function Encode
        (Data : UCD.Code_Point_Vectors.Vector)
         return UTF_8_Code_Unit_Vectors.Vector;

      package Compressed_Stage_Table is
        new Generic_Compressed_Stage_Table (Mapping_Record, Mapping_Array);

      Compressed :
        array (Case_Mapping) of Compressed_Stage_Table.Compressed_Stage_Table;

      -----------------
      -- Append_Data --
      -----------------

      function Append_Data
        (Data : UCD.Code_Point_Vectors.Vector) return Mapping_Record
      is
         Encoded : constant UTF_8_Code_Unit_Vectors.Vector := Encode (Data);
         Found   : Boolean := False;
         Offset  : Unsigned_14;

      begin
         Max_Length := Natural'Max (Max_Length, Natural (Data.Length));
         Max_UTF_8  := Natural'Max (Max_UTF_8, Natural (Encoded.Length));

         UTF_8_Data_Size := UTF_8_Data_Size + Natural (Encoded.Length);

         for Position in UTF_8_Data.First_Index .. UTF_8_Data.Last_Index loop
            if UTF_8_Data.Element (Position) = Encoded.First_Element then
               Found := True;
               Offset := Unsigned_14 (Position - 1);

               for Offset in 1 .. Natural (Encoded.Length) - 1 loop
                  if UTF_8_Data.Element (Position + Offset)
                    /= Encoded.Element (Encoded.First_Index + Offset)
                  then
                     Found := False;

                     exit;
                  end if;
               end loop;
            end if;
         end loop;

         if not Found then
            Offset := Unsigned_14 (UTF_8_Data.Length);
            UTF_8_Data.Append_Vector (Encoded);
         end if;

         return
           (Offset      => Offset,
            Length      => Unsigned_2 (Data.Length),
            Size        => Unsigned_3 (Encoded.Length),
            Has_Mapping => True,
            others      => <>);
      end Append_Data;

      --------------
      -- Compress --
      --------------

      procedure Compress is
      begin
         for J in Case_Mapping'Range loop
            Compressed (J).Build (Raw_Mapping (J).all);
         end loop;
      end Compress;

      ------------
      -- Encode --
      ------------

      function Encode
        (Data : UCD.Code_Point_Vectors.Vector)
         return UTF_8_Code_Unit_Vectors.Vector
      is
         use type Interfaces.Unsigned_32;

         C  : Interfaces.Unsigned_32;

      begin
         return Result : UTF_8_Code_Unit_Vectors.Vector do
            for Position in Data.First_Index .. Data.Last_Index loop
               C := Interfaces.Unsigned_32 (Data.Element (Position));

               if C <= 16#00_007F# then
                  Result.Append (UTF_8_Code_Unit (C));

               elsif C <= 16#00_07FF# then
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1100_0000#
                        or ((C and 2#111_1100_0000#) / 2#100_0000#)));
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1000_0000#
                        or (C and 2#000_0011_1111#)));

               elsif C <= 16#00_FFFF# then
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1110_0000#
                        or ((C and 2#1111_0000_0000_0000#)
                          / 2#1_0000_0000_0000#)));
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1000_0000#
                        or ((C and 2#0000_1111_1100_0000#) / 2#100_0000#)));
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1000_0000# or (C and 2#0000_0000_0011_1111#)));

               else
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1111_0000#
                        or ((C and 2#1_1100_0000_0000_0000_0000#)
                          / 2#100_0000_0000_0000_0000#)));
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1000_0000#
                        or ((C and 2#0_0011_1111_0000_0000_0000#)
                          / 2#1_0000_0000_0000#)));
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1000_0000#
                        or ((C and 2#0_0000_0000_1111_1100_0000#)
                          / 2#100_0000#)));
                  Result.Append
                    (UTF_8_Code_Unit
                       (2#1000_0000#
                        or (C and 2#0_0000_0000_0000_0011_1111#)));
               end if;
            end loop;
         end return;
      end Encode;

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
            & Natural'Wide_Wide_Image (UTF_8_Data_Size)
            & " bytes (compressed size is"
            & Natural'Wide_Wide_Image (Natural (UTF_8_Data.Length))
            & " bytes)");
      end Print_Statistics;

      ---------
      -- Set --
      ---------

      procedure Set
        (Character : UCD.Code_Point;
         Mapping   : Case_Mapping;
         Data      : UCD.Code_Point_Vectors.Vector) is
      begin
         Raw_Mapping (Mapping) (Character) := Append_Data (Data);
      end Set;

      ----------------
      -- Set_NFD_QC --
      ----------------

      procedure Set_NFD_QC
        (Character : UCD.Code_Point;
         To        : Boolean) is
      begin
         for Mapping in Case_Mapping loop
            Raw_Mapping (Mapping) (Character).NFD_QC := To;
         end loop;
      end Set_NFD_QC;

      ------------------------
      -- UTF_8_Data_Element --
      ------------------------

      function UTF_8_Data_Element (Index : Natural) return Unsigned_8 is
      begin
         return Unsigned_8 (UTF_8_Data.Element (Index + 1));
      end UTF_8_Data_Element;

      ---------------------------
      -- UTF_8_Data_Index_Last --
      ---------------------------

      function UTF_8_Data_Index_Last return Natural is
      begin
         return Natural (UTF_8_Data.Last_Index) - 1;
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
         Put_Line
           (File,
            "     constant array (Mapping_Group) of Mapping_Data_Offset :=");

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
         & Positive'Wide_Wide_Image (Database.UTF_8_Data_Index_Last) & ";");
      New_Line (File);

      Put_Line
        (File,
         "   subtype Casing_Character_Count is");
      Put_Line
        (File,
         "     VSS.Implementation.Strings.Character_Count range 0 .. 3;");
      New_Line (File);

      Put_Line
        (File,
         "   subtype Casing_UTF8_Code_Unit_Count is");
      Put_Line
        (File,
         "     VSS.Unicode.UTF8_Code_Unit_Count range 0 .. 6;");
      New_Line (File);

      Put_Line
        (File,
         "   type Mapping_Information is record");
      Put_Line
        (File,
         "      Offset      : Casing_UTF8_Data_Offset;");
      Put_Line
        (File,
         "      Length      : Casing_Character_Count;");
      Put_Line
        (File,
         "      Count       : Casing_UTF8_Code_Unit_Count;");
      Put_Line
        (File,
         "      NFD_QC      : Boolean;");
      Put_Line
        (File,
         "      Has_Mapping : Boolean;");
      Put_Line (File, "   end record;");
      Put_Line
        (File,
         "   for Mapping_Information'Size use 32;");
      Put_Line
        (File,
         "   for Mapping_Information use record");
      Put_Line
        (File,
         "      Offset      at 0 range 0 .. 13;");
      Put_Line
        (File,
         "      Length      at 0 range 16 .. 17;");
      Put_Line
        (File,
         "      Count       at 0 range 24 .. 26;");
      Put_Line
        (File,
         "      NFD_QC      at 0 range 30 .. 30;");
      Put_Line
        (File,
         "      Has_Mapping at 0 range 31 .. 31;");
      Put_Line (File, "   end record;");
      New_Line (File);

      Put_Line
        (File,
         "   type Mapping_Data_Offset is range 0 .."
         & Integer'Wide_Wide_Image (Database.Mapping_Data_Last)
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
         "   Group_Size : constant :="
         & Integer'Wide_Wide_Image
           (Database.Mapping_Index_Group_Size (Database.Simple_Lowercase))
         & ";");
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
            "     constant array (Casing_UTF8_Data_Offset)");
         Put_Line
           (File,
            "       of VSS.Unicode.UTF8_Code_Unit :=");

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
