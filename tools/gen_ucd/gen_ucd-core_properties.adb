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

with Ada.Containers.Hashed_Maps;
with Ada.Integer_Wide_Wide_Text_IO;     use Ada.Integer_Wide_Wide_Text_IO;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;       use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;
with Interfaces;

with Gen_UCD.Unsigned_Types;
with UCD.Characters;
with UCD.Properties;

package body Gen_UCD.Core_Properties is

   function Minimum_Bits (Value : Integer) return Integer;

   function Value_Identifier
     (Property : not null UCD.Properties.Property_Access;
      Value    : not null UCD.Properties.Property_Value_Access)
      return Wide_Wide_String;

   package Property_Value_Integer_Maps is
     new Ada.Containers.Hashed_Maps
       (UCD.Properties.Property_Value_Access,
        Integer,
        UCD.Properties.Hash,
        UCD.Properties."=");

   GC_Mapping  : Property_Value_Integer_Maps.Map;
   GCB_Mapping : Property_Value_Integer_Maps.Map;

   package Database is

      procedure Initialize (Record_Size : Positive);

      procedure Compress;

      procedure Set_GC
        (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_Types.Unsigned_5);

      procedure Set_OLower (Code : UCD.Code_Point; To : Boolean);

      procedure Set_OUpper (Code : UCD.Code_Point; To : Boolean);

      procedure Set_ExtPict (Code : UCD.Code_Point; To : Boolean);

      procedure Set_GCB
        (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_Types.Unsigned_4);

      function Uncompressed_Size return Positive;

      function Block_Size return Positive;

      function Data_Size return Positive;

      function Index_Last return Positive;

      function Data_Index_Last return Positive;

      function Index_Table_Element (Index : Natural) return Natural;

      function Data_Table_Element
        (Index : Natural) return Gen_UCD.Unsigned_Types.Unsigned_16;

   end Database;

   -----------
   -- Build --
   -----------

   procedure Build is
      GC_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("gc");
      GCB_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("GCB");

   begin
      Put ("   ... core properties");

      declare
         Count : Natural := 0;

      begin
         for Value of GC_Property.All_Values loop
            if Value.Is_Used then
               GC_Mapping.Insert (Value, Count);
               Count := Count + 1;
            end if;
         end loop;
      end;

      declare
         Count : Natural := 0;

      begin
         for Value of GCB_Property.All_Values loop
            if Value.Is_Used then
               GCB_Mapping.Insert (Value, Count);
               Count := Count + 1;
            end if;
         end loop;
      end;

      Database.Initialize (8);

      declare
         use type UCD.Properties.Property_Value_Access;

         OLower_Property  : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("OLower");
         OLower_Y         :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (OLower_Property, "Y");
         OUpper_Property  : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("OUpper");
         OUpper_Y         :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (OUpper_Property, "Y");
         ExtPict_Property : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("ExtPict");
         ExtPict_Y        :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (ExtPict_Property, "Y");

      begin
         for Code in UCD.Code_Point loop
            Database.Set_GC
              (Code,
               Gen_UCD.Unsigned_Types.Unsigned_5
                 (GC_Mapping.Element
                      (UCD.Characters.Get (Code, GC_Property))));

            Database.Set_OLower
              (Code,
               UCD.Characters.Get (Code, OLower_Property) = OLower_Y);
            Database.Set_OUpper
              (Code,
               UCD.Characters.Get (Code, OUpper_Property) = OUpper_Y);
            Database.Set_ExtPict
              (Code,
               UCD.Characters.Get (Code, ExtPict_Property) = ExtPict_Y);

            Database.Set_GCB
              (Code,
               Gen_UCD.Unsigned_Types.Unsigned_4
                 (GCB_Mapping.Element
                      (UCD.Characters.Get (Code, GCB_Property))));
         end loop;
      end;

      Database.Compress;

      Put_Line
        (" (uncompressed"
         & Integer'Wide_Wide_Image (Database.Uncompressed_Size)
         & " bytes, compressed"
         & Integer'Wide_Wide_Image (Database.Data_Size)
         & " bytes, block size"
         & Integer'Wide_Wide_Image (Database.Block_Size)
         & " rows)");
   end Build;

   --------------
   -- Database --
   --------------

   package body Database is

      use type Gen_UCD.Unsigned_Types.Unsigned_32;

      --  GC, OLower, OUpper are used for character classification in public
      --  API, and put into the first byte. One bit in this byte is reserved.
      --
      --  GCB & ExtPict are used by grapheme cluster iterator and put into
      --  second byte of the record. Three bits in this byte are reserved.

      type Core_Data_Record is record
         GC         : Gen_UCD.Unsigned_Types.Unsigned_5 := 0;
         OLower     : Gen_UCD.Unsigned_Types.Unsigned_1 := 0;
         OUpper     : Gen_UCD.Unsigned_Types.Unsigned_1 := 0;
         Reserved_1 : Gen_UCD.Unsigned_Types.Unsigned_1 := 0;
         GCB        : Gen_UCD.Unsigned_Types.Unsigned_4 := 0;
         ExtPict    : Gen_UCD.Unsigned_Types.Unsigned_1 := 0;
         Reserved_2 : Gen_UCD.Unsigned_Types.Unsigned_3 := 0;
      end record;
      for Core_Data_Record'Size use 16;
      for Core_Data_Record use record
         GC         at 0 range 0 .. 4;
         OLower     at 0 range 5 .. 5;
         OUpper     at 0 range 6 .. 6;
         Reserved_1 at 0 range 7 .. 7;
         GCB        at 0 range 8 .. 11;
         ExtPict    at 0 range 12 .. 12;
         Reserved_2 at 0 range 13 .. 15;
      end record;

      type Core_Data_Array is
        array (Gen_UCD.Unsigned_Types.Unsigned_32 range <>)
          of Core_Data_Record;

      type Core_Data_Array_Access is access all Core_Data_Array;

      type Unsigned_32_Array is
        array (Gen_UCD.Unsigned_Types.Unsigned_32 range <>)
          of Gen_UCD.Unsigned_Types.Unsigned_32;

      type Unsigned_32_Array_Access is access all Unsigned_32_Array;

      Raw         : Core_Data_Array_Access;

      Compressed_Block_Size : Gen_UCD.Unsigned_Types.Unsigned_32;
      Compressed_Data       : Core_Data_Array_Access;
      Compressed_Data_Last  : Gen_UCD.Unsigned_Types.Unsigned_32;
      Index_Data            : Unsigned_32_Array_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Core_Data_Array, Core_Data_Array_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Unsigned_32_Array, Unsigned_32_Array_Access);

      procedure Compress
        (Block_Size : Gen_UCD.Unsigned_Types.Unsigned_32;
         Done       : in out Boolean);

      function Memory_Consumption
        (Compressed_Data_Last : Gen_UCD.Unsigned_Types.Unsigned_32;
         Index_Data           : Unsigned_32_Array) return Integer;

      ----------------
      -- Block_Size --
      ----------------

      function Block_Size return Positive is
      begin
         return Positive (Compressed_Block_Size);
      end Block_Size;

      --------------
      -- Compress --
      --------------

      procedure Compress is
         Done : Boolean := False;

      begin
         --  Compress (2, Done);
         --  Compress (4, Done);
         --  Compress (8, Done);
         --  Compress (16, Done);
         --  Compress (32, Done);
         --  Compress (64, Done);
         --  Compress (128, Done);
         Compress (256, Done);
         --  Compress (512, Done);
         --  Compress (1024, Done);
         --  Compress (2048, Done);
      end Compress;

      --------------
      -- Compress --
      --------------

      procedure Compress
        (Block_Size : Gen_UCD.Unsigned_Types.Unsigned_32;
         Done       : in out Boolean)
      is

         function Is_Equal
           (Raw_Block  : Gen_UCD.Unsigned_Types.Unsigned_32;
            Compressed : Gen_UCD.Unsigned_Types.Unsigned_32) return Boolean;

         --------------
         -- Is_Equal --
         --------------

         function Is_Equal
           (Raw_Block  : Gen_UCD.Unsigned_Types.Unsigned_32;
            Compressed : Gen_UCD.Unsigned_Types.Unsigned_32) return Boolean is
         begin
            return
              Raw (Raw_Block * Block_Size .. (Raw_Block + 1) * Block_Size - 1)
                = Compressed_Data (Compressed .. Compressed + Block_Size - 1);
         end Is_Equal;

         Previous_Compressed_Block_Size : constant
           Gen_UCD.Unsigned_Types.Unsigned_32 :=
             Compressed_Block_Size;
         Previous_Compressed_Data       : Core_Data_Array_Access :=
           Compressed_Data;
         Previous_Compressed_Data_Last  : constant
           Gen_UCD.Unsigned_Types.Unsigned_32 :=
             Compressed_Data_Last;
         Previous_Index_Data            : Unsigned_32_Array_Access :=
           Index_Data;

         Reused : Boolean;

      begin
         if Done then
            return;
         end if;

         Compressed_Block_Size := Block_Size;
         Compressed_Data := new Core_Data_Array (Raw'Range);
         Index_Data :=
           new Unsigned_32_Array (0 .. Raw'Length / Block_Size - 1);

         --  Copy first block

         Compressed_Data (0 .. Block_Size - 1) := Raw (0 .. Block_Size - 1);
         Compressed_Data_Last := Block_Size - 1;
         Index_Data (0) := 0;

         --  Process all other blocks

         for Block in 1 .. Raw'Length / Block_Size - 1 loop
            Reused := False;

            for Compressed in 0 .. Compressed_Data_Last - Block_Size + 1 loop
               if Is_Equal (Block, Compressed) then
                  Index_Data (Block) := Compressed;
                  Reused := True;

                  exit;
               end if;
            end loop;

            if not Reused then
               Index_Data (Block) := Compressed_Data_Last + 1;
               Compressed_Data_Last := Compressed_Data_Last + Block_Size;
               Compressed_Data
                 (Compressed_Data_Last - Block_Size + 1
                  .. Compressed_Data_Last) :=
                 Raw (Block * Block_Size .. (Block + 1) * Block_Size - 1);
            end if;
         end loop;

         --  Compare results with previous iteration.

         if Previous_Compressed_Data /= null
           and then Memory_Consumption
             (Previous_Compressed_Data_Last, Previous_Index_Data.all)
           <= Memory_Consumption (Compressed_Data_Last, Index_Data.all)
         then
            Done := True;

            Free (Compressed_Data);
            Free (Index_Data);

            Compressed_Block_Size := Previous_Compressed_Block_Size;
            Compressed_Data       := Previous_Compressed_Data;
            Compressed_Data_Last  := Previous_Compressed_Data_Last;
            Index_Data            := Previous_Index_Data;

         else
            Free (Previous_Compressed_Data);
            Free (Previous_Index_Data);
         end if;
      end Compress;

      ---------------------
      -- Data_Index_Last --
      ---------------------

      function Data_Index_Last return Positive is
      begin
         return Positive (Compressed_Data_Last);
      end Data_Index_Last;

      ---------------
      -- Data_Size --
      ---------------

      function Data_Size return Positive is
      begin
         return
           Positive
             (Memory_Consumption (Compressed_Data_Last, Index_Data.all));
      end Data_Size;

      ------------------------
      -- Data_Table_Element --
      ------------------------

      function Data_Table_Element
        (Index : Natural) return Gen_UCD.Unsigned_Types.Unsigned_16
      is
         function To_Unsigned_8 is
           new Ada.Unchecked_Conversion
                 (Core_Data_Record, Gen_UCD.Unsigned_Types.Unsigned_16);

      begin
         return
           To_Unsigned_8
             (Compressed_Data
                (Gen_UCD.Unsigned_Types.Unsigned_32 (Index)));
      end Data_Table_Element;

      ----------------
      -- Index_Last --
      ----------------

      function Index_Last return Positive is
      begin
         return Positive (Index_Data'Last);
      end Index_Last;

      -------------------------
      -- Index_Table_Element --
      -------------------------

      function Index_Table_Element (Index : Natural) return Natural is
      begin
         return
           Natural (Index_Data (Gen_UCD.Unsigned_Types.Unsigned_32 (Index)));
      end Index_Table_Element;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Record_Size : Positive) is
      begin
         if Record_Size /= 8 then
            raise Program_Error;
         end if;

         Raw :=
           new Core_Data_Array
             (0 .. Gen_UCD.Unsigned_Types.Unsigned_32 (UCD.Code_Point'Last));
      end Initialize;

      ------------------------
      -- Memory_Consumption --
      ------------------------

      function Memory_Consumption
        (Compressed_Data_Last : Gen_UCD.Unsigned_Types.Unsigned_32;
         Index_Data           : Unsigned_32_Array) return Integer is
      begin
         return
           Integer
             ((Compressed_Data_Last + 1) * 2
              + (if Compressed_Data_Last
                   <= Gen_UCD.Unsigned_Types.Unsigned_32
                        (Gen_UCD.Unsigned_Types.Unsigned_16'Last)
                then Index_Data'Length * 2
                else Index_Data'Length * 4));
      end Memory_Consumption;

      -----------------
      -- Set_ExtPict --
      -----------------

      procedure Set_ExtPict (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_Types.Unsigned_32 (Code)).ExtPict :=
           Boolean'Pos (To);
      end Set_ExtPict;

      ------------
      -- Set_GC --
      ------------

      procedure Set_GC
        (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_Types.Unsigned_5) is
      begin
         Raw (Gen_UCD.Unsigned_Types.Unsigned_32 (Code)).GC := To;
      end Set_GC;

      -------------
      -- Set_GCB --
      -------------

      procedure Set_GCB
        (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_Types.Unsigned_4) is
      begin
         Raw (Gen_UCD.Unsigned_Types.Unsigned_32 (Code)).GCB := To;
      end Set_GCB;

      ----------------
      -- Set_OLower --
      ----------------

      procedure Set_OLower (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_Types.Unsigned_32 (Code)).OLower :=
           Boolean'Pos (To);
      end Set_OLower;

      ----------------
      -- Set_OUpper --
      ----------------

      procedure Set_OUpper (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_Types.Unsigned_32 (Code)).OUpper :=
           Boolean'Pos (To);
      end Set_OUpper;

      -----------------------
      -- Uncompressed_Size --
      -----------------------

      function Uncompressed_Size return Positive is
      begin
         return Raw.all'Length;
      end Uncompressed_Size;

   end Database;

   --------------
   -- Generate --
   --------------

   procedure Generate (File : Ada.Wide_Wide_Text_IO.File_Type) is
   begin
      Put_Line ("   ... core properties");

      Put_Line (File, "pragma Restrictions (No_Elaboration_Code);");
      New_Line (File);

      Put_Line (File, "with Interfaces;");
      New_Line (File);
      Put_Line (File, "package VSS.Implementation.UCD_Core is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      New_Line (File);

      --  Generate GC_Values type

      declare
         Property : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("gc");
         First    : Boolean := True;
         Count    : Natural := 0;

      begin
         Put_Line (File, "   type GC_Values is");

         for Value of Property.All_Values loop
            if Value.Is_Used then
               Count := Count + 1;

               if First then
                  Put (File, "     (");
                  First := False;

               else
                  Put_Line (File, ",");
                  Put (File, "      ");
               end if;

               Put (File, Value_Identifier (Property, Value));
            end if;
         end loop;

         Put_Line (File, ");");

         Put_Line
           (File,
            "   for GC_Values'Size use"
            & Natural'Wide_Wide_Image (Minimum_Bits (Count))
            & ";");
         Put_Line (File, "   for GC_Values use");
         First := True;

         for Value of Property.All_Values loop
            if Value.Is_Used then
               Count := Count + 1;

               if First then
                  Put (File, "     (");
                  First := False;

               else
                  Put_Line (File, ",");
                  Put (File, "      ");
               end if;

               Put (File, Value_Identifier (Property, Value));
               Put (File, " =>");
               Put
                 (File, Integer'Wide_Wide_Image (GC_Mapping.Element (Value)));
            end if;
         end loop;

         Put_Line (File, ");");
         New_Line (File);
      end;

      --  Generate GCB_Values type

      declare
         Property : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("GCB");
         First    : Boolean := True;
         Count    : Natural := 0;

      begin
         Put_Line (File, "   type GCB_Values is");

         for Value of Property.All_Values loop
            if Value.Is_Used then
               Count := Count + 1;

               if First then
                  Put (File, "     (");
                  First := False;

               else
                  Put_Line (File, ",");
                  Put (File, "      ");
               end if;

               Put (File, Value_Identifier (Property, Value));
            end if;
         end loop;

         Put_Line (File, ");");

         Put_Line
           (File,
            "   for GCB_Values'Size use"
            & Natural'Wide_Wide_Image (Minimum_Bits (Count))
            & ";");
         Put_Line (File, "   for GCB_Values use");
         First := True;

         for Value of Property.All_Values loop
            if Value.Is_Used then
               Count := Count + 1;

               if First then
                  Put (File, "     (");
                  First := False;

               else
                  Put_Line (File, ",");
                  Put (File, "      ");
               end if;

               Put (File, Value_Identifier (Property, Value));
               Put (File, " =>");
               Put
                 (File, Integer'Wide_Wide_Image (GCB_Mapping.Element (Value)));
            end if;
         end loop;

         Put_Line (File, ");");
         New_Line (File);
      end;

      --  Generate types for index and data tables.

      declare
         Index_Last_Image : Wide_Wide_String (1 .. 10);
         Data_Last_Image  : Wide_Wide_String (1 .. 10);
         Block_Image      : Wide_Wide_String (1 .. 10);

      begin
         Put (Index_Last_Image, Database.Index_Last, 16);
         Put (Block_Image, Database.Block_Size, 16);
         Put (Data_Last_Image, Database.Data_Index_Last, 16);

         Put_Line
           (File,
            "   type Core_Index is range 0 .. "
            & Trim (Index_Last_Image, Both)
            & ";");
         New_Line (File);
         Put_Line
           (File,
            "   Block_Size : constant := " & Trim (Block_Image, Both) & ";");
         New_Line (File);
         Put_Line
           (File,
            "   type Core_Offset is range 0 .. "
            & Trim (Data_Last_Image, Both)
            & ";");
         New_Line (File);

         Put_Line
           (File,
            "   type Core_Data_Record is record");
         Put_Line (File, "      GC      : GC_Values;");
         Put_Line (File, "      OLower  : Boolean;");
         Put_Line (File, "      OUpper  : Boolean;");
         Put_Line (File, "      GCB     : GCB_Values;");
         Put_Line (File, "      ExtPict : Boolean;");
         Put_Line (File, "   end record;");
         Put_Line (File, "   for Core_Data_Record'Size use 16;");
         Put_Line (File, "   for Core_Data_Record use record");
         Put_Line (File, "      GC      at 0 range 0 .. 4;");
         Put_Line (File, "      OLower  at 0 range 5 .. 5;");
         Put_Line (File, "      OUpper  at 0 range 6 .. 6;");
         Put_Line (File, "      GCB     at 0 range 8 .. 11;");
         Put_Line (File, "      ExtPict at 0 range 12 .. 12;");
         Put_Line (File, "   end record;");
         New_Line (File);

         Put_Line
           (File,
            "   type Index_Table_Array is array (Core_Index) of Core_Offset;");
         Put_Line (File, "   pragma Pack (Index_Table_Array);");
         New_Line (File);

         Put_Line
           (File,
            "   type Core_Data_Array is"
            & " array (Core_Offset) of Core_Data_Record;");
         Put_Line (File, "   pragma Pack (Core_Data_Array);");
         New_Line (File);

         Put_Line
           (File,
            "   type Core_Data_Raw_Array is"
            & " array (Core_Offset) of Interfaces.Unsigned_16;");
         Put_Line (File, "   pragma Pack (Core_Data_Raw_Array);");
         New_Line (File);
      end;

      --  Generate index table.

      declare
         Image : Wide_Wide_String (1 .. 10);
         First : Boolean := True;

      begin
         Put_Line
           (File,
            "   Core_Index_Table : constant Index_Table_Array :=");

         for J in 0 .. Database.Index_Last loop
            if First then
               First := False;
               Put (File, "     (");

            elsif J mod 8 = 0 then
               Put_Line (File, ",");
               Put (File, "      ");

            else
               Put (File, ", ");
            end if;

            Put (Image, Database.Index_Table_Element (J));
            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, ");");
         New_Line (File);
      end;

      --  Generate data table.

      declare
         Image : Wide_Wide_String (1 .. 10);

      begin
         Put_Line
           (File,
            "   Core_Data_Raw_Table : constant Core_Data_Raw_Array :=");

         for J in 0 .. Database.Data_Index_Last loop
            Put (Image, Integer (Database.Data_Table_Element (J)), 16);

            if J = 0 then
               Put (File, "     (");

            elsif J mod 6 = 0 then
               Put_Line (File, ",");
               Put (File, "      ");

            else
               Put (File, ", ");
            end if;

            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, ");");
         New_Line (File);

         Put_Line
           (File,
            "   Core_Data_Table : constant Core_Data_Array");
         Put_Line
           (File,
            "     with Import, Convention => Ada,"
            & " Address => Core_Data_Raw_Table'Address;");
         New_Line (File);
      end;

      Put_Line (File, "end VSS.Implementation.UCD_Core;");
   end Generate;

   ------------------
   -- Minimum_Bits --
   ------------------

   function Minimum_Bits (Value : Integer) return Integer is
      use type Interfaces.Unsigned_32;

      Aux : Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Value);

   begin
      return Result : Integer := 32 do
         loop
            exit when Aux / 16#8000_0000# = 1;

            Result := Result - 1;
            Aux    := Aux * 2;
         end loop;
      end return;
   end Minimum_Bits;

   ----------------------
   -- Value_Identifier --
   ----------------------

   function Value_Identifier
     (Property : not null UCD.Properties.Property_Access;
      Value    : not null UCD.Properties.Property_Value_Access)
      return Wide_Wide_String
   is
      Property_Name : constant Wide_Wide_String :=
        To_Upper (To_Wide_Wide_String (Property.Names.First_Element));
      Value_Name    : constant Wide_Wide_String :=
        To_Wide_Wide_String (Value.Names.First_Element);

   begin
      return Property_Name & '_' & Value_Name;
   end Value_Identifier;

end Gen_UCD.Core_Properties;
