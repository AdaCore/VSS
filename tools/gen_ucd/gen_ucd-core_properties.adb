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
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;       use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;
with Ada.Integer_Wide_Wide_Text_IO;     use Ada.Integer_Wide_Wide_Text_IO;
with Interfaces;

with Gen_UCD.Characters;
with Gen_UCD.Properties;

package body Gen_UCD.Core_Properties is

   function Minimum_Bits (Value : Integer) return Integer;

   function Value_Identifier
     (Property : not null Gen_UCD.Properties.Property_Access;
      Value    : not null Gen_UCD.Properties.Property_Value_Access)
      return Wide_Wide_String;

   package Property_Value_Integer_Maps is
     new Ada.Containers.Hashed_Maps
       (Gen_UCD.Properties.Property_Value_Access,
        Integer,
        Gen_UCD.Properties.Hash,
        Gen_UCD.Properties."=");

   GC_Mapping : Property_Value_Integer_Maps.Map;

   package Database is

      --  type Field_5 is (Field_5_1);

      type Unsigned_5 is mod 2 ** 5;
      for Unsigned_5'Size use 5;

      procedure Initialize (Record_Size : Positive);

      procedure Compress;

      procedure Set_5 (Code : Code_Point; To : Unsigned_5);

      function Uncompressed_Size return Positive;

      function Block_Size return Positive;

      function Data_Size return Positive;

      function Index_Last return Positive;

      function Data_Index_Last return Positive;

      function Index_Table_Element (Index : Natural) return Natural;

      function Data_Table_Element
        (Index : Natural) return Interfaces.Unsigned_8;

   end Database;

   -----------
   -- Build --
   -----------

   procedure Build is
   begin
      Put ("   ... core properties");

      declare
         Property : constant not null Gen_UCD.Properties.Property_Access :=
           Gen_UCD.Properties.Resolve ("gc");
         Count    : Natural := 0;

      begin
         for Value of Property.All_Values loop
            if Value.Is_Used then
               GC_Mapping.Insert (Value, Count);
               Count := Count + 1;
            end if;
         end loop;
      end;

      Database.Initialize (8);

      declare
         Property : constant not null Gen_UCD.Properties.Property_Access :=
           Gen_UCD.Properties.Resolve ("gc");

      begin
         for Code in Code_Point loop
            Database.Set_5
              (Code,
               Database.Unsigned_5
                 (GC_Mapping.Element
                      (Gen_UCD.Characters.Get (Code, Property))));
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

      type Unsigned_8 is mod 2 ** 8;
      for Unsigned_8'Size use 8;

      type Unsigned_16 is mod 2 ** 16;
      for Unsigned_16'Size use 16;

      type Unsigned_32 is mod 2 ** 32;
      for Unsigned_32'Size use 32;

      type Field_Size is (F5, F8);

      type Union_8 (Size : Field_Size := F5) is record
         case Size is
            when F5 =>
               F5_1 : Unsigned_5;

            when F8 =>
               F8_1 : Unsigned_8;
         end case;
      end record;
      pragma Unchecked_Union (Union_8);
      for Union_8'Size use 8;

      type Union_8_Array is array (Unsigned_32 range <>) of Union_8;

      type Union_8_Array_Access is access all Union_8_Array;

      type Unsigned_32_Array is array (Unsigned_32 range <>) of Unsigned_32;

      type Unsigned_32_Array_Access is access all Unsigned_32_Array;

      Raw         : Union_8_Array_Access;
      Record_Size : Unsigned_32;

      Compressed_Block_Size : Unsigned_32;
      Compressed_Data       : Union_8_Array_Access;
      Compressed_Data_Last  : Unsigned_32;
      Index_Data            : Unsigned_32_Array_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation (Union_8_Array, Union_8_Array_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Unsigned_32_Array, Unsigned_32_Array_Access);

      procedure Compress (Block_Size : Unsigned_32; Done : in out Boolean);

      function Memory_Consumption
        (Compressed_Data_Last : Unsigned_32;
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

      procedure Compress (Block_Size : Unsigned_32; Done : in out Boolean) is

         function Is_Equal
           (Raw_Block  : Unsigned_32;
            Compressed : Unsigned_32) return Boolean;

         --------------
         -- Is_Equal --
         --------------

         function Is_Equal
           (Raw_Block  : Unsigned_32;
            Compressed : Unsigned_32) return Boolean is
         begin
            for Offset in 0 .. Block_Size - 1 loop
               if Raw (Raw_Block * Block_Size + Offset).F8_1
                 /= Compressed_Data (Compressed + Offset).F8_1
               then
                  return False;
               end if;
            end loop;

            return True;
         end Is_Equal;

         Previous_Compressed_Block_Size : constant Unsigned_32 :=
           Compressed_Block_Size;
         Previous_Compressed_Data       : Union_8_Array_Access :=
           Compressed_Data;
         Previous_Compressed_Data_Last  : constant  Unsigned_32 :=
           Compressed_Data_Last;
         Previous_Index_Data            : Unsigned_32_Array_Access :=
           Index_Data;

         Reused : Boolean;

      begin
         if Done then
            return;
         end if;

         Compressed_Block_Size := Block_Size;
         Compressed_Data := new Union_8_Array (Raw'Range);
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
        (Index : Natural) return Interfaces.Unsigned_8 is
      begin
         return
           Interfaces.Unsigned_8 (Compressed_Data (Unsigned_32 (Index)).F8_1);
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
         return Natural (Index_Data (Unsigned_32 (Index)));
      end Index_Table_Element;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Record_Size : Positive) is
      begin
         if Record_Size /= 8 then
            raise Program_Error;
         end if;

         Database.Record_Size := 1;
         Raw :=
           new Union_8_Array'
             (0 .. Unsigned_32 (Code_Point'Last) * Database.Record_Size =>
                (F8, 0));
      end Initialize;

      ------------------------
      -- Memory_Consumption --
      ------------------------

      function Memory_Consumption
        (Compressed_Data_Last : Unsigned_32;
         Index_Data           : Unsigned_32_Array) return Integer is
      begin
         return
           Integer
             (Compressed_Data_Last + 1
              + (if Compressed_Data_Last <= Unsigned_32 (Unsigned_16'Last)
                then Index_Data'Length * 2
                else Index_Data'Length * 4));
      end Memory_Consumption;

      -----------
      -- Set_5 --
      -----------

      procedure Set_5 (Code : Code_Point; To : Unsigned_5) is
      begin
         Raw (Unsigned_32 (Code) * Record_Size).F5_1 := To;
      end Set_5;

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

      Put_Line (File, "with Interfaces;");
      New_Line (File);
      Put_Line (File, "package VSS.Implementation.UCD_Core is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      New_Line (File);

      --  Generate GC_Values type

      declare
         Property : constant not null Gen_UCD.Properties.Property_Access :=
           Gen_UCD.Properties.Resolve ("gc");
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
            "   type Core_Data_Record (Raw : Boolean := False) is record");
         Put_Line (File, "      case Raw is");
         Put_Line (File, "         when True =>");
         Put_Line (File, "            Data : Interfaces.Unsigned_8;");
         New_Line (File);
         Put_Line (File, "         when False =>");
         Put_Line (File, "            GC : GC_Values;");
         Put_Line (File, "      end case;");
         Put_Line (File, "   end record;");
         Put_Line (File, "   pragma Unchecked_Union (Core_Data_Record);");
         Put_Line (File, "   for Core_Data_Record use record");
         Put_Line (File, "      Data at 0 range 0 .. 7;");
         Put_Line (File, "      GC   at 0 range 0 .. 4;");
         Put_Line (File, "   end record;");
         New_Line (File);
      end;

      --  Generate index table.

      declare
         Image : Wide_Wide_String (1 .. 10);
         First : Boolean := True;

      begin
         Put_Line
           (File,
            "   Core_Index_Table :"
            & " constant array (Core_Index) of Core_Offset :=");

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
            "   Core_Data_Table : "
            & "constant array (Core_Offset) of Core_Data_Record :=");

         for J in 0 .. Database.Data_Index_Last loop
            Put (Image, Integer (Database.Data_Table_Element (J)), 16);

            if J = 0 then
               Put (File, "     (");

            elsif J mod 4 = 0 then
               Put_Line (File, ",");
               Put (File, "      ");

            else
               Put (File, ", ");
            end if;

            Put (File, "(True, " & Trim (Image, Both) & ")");
         end loop;

         Put_Line (File, ");");
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
     (Property : not null Gen_UCD.Properties.Property_Access;
      Value    : not null Gen_UCD.Properties.Property_Value_Access)
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
