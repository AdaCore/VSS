--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Integer_Wide_Wide_Text_IO;     use Ada.Integer_Wide_Wide_Text_IO;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed;       use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;

with UCD.Characters;
with UCD.Properties;

with Gen_UCD.Enumeration_Types;

package body Gen_UCD.Core_Properties is

   GC_Enumeration   : Gen_UCD.Enumeration_Types.Enumeration_Type;
   GCB_Enumeration  : Gen_UCD.Enumeration_Types.Enumeration_Type;
   WB_Enumeration   : Gen_UCD.Enumeration_Types.Enumeration_Type;
   InCB_Enumeration : Gen_UCD.Enumeration_Types.Enumeration_Type;
   EA_Enumeration   : Gen_UCD.Enumeration_Types.Enumeration_Type;

   package Database is

      procedure Initialize (Record_Size : Positive);

      procedure Compress;

      procedure Set_GC (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_5);

      procedure Set_OLower (Code : UCD.Code_Point; To : Boolean);

      procedure Set_OUpper (Code : UCD.Code_Point; To : Boolean);

      procedure Set_ExtPict (Code : UCD.Code_Point; To : Boolean);

      procedure Set_GCB (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_4);

      procedure Set_WB (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_5);

      procedure Set_InCB (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_2);

      procedure Set_EA (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_3);

      procedure Set_Emoji (Code : UCD.Code_Point; To : Boolean);

      procedure Set_EPres (Code : UCD.Code_Point; To : Boolean);

      procedure Set_EBase (Code : UCD.Code_Point; To : Boolean);

      procedure Set_EMod (Code : UCD.Code_Point; To : Boolean);

      function Uncompressed_Size return Positive;

      function Block_Size return Positive;

      function Data_Size return Positive;

      function Index_Last return Positive;

      function Data_Index_Last return Positive;

      function Index_Table_Element (Index : Natural) return Natural;

      function Data_Table_Element (Index : Natural) return Gen_UCD.Unsigned_32;

   end Database;

   RI_First : UCD.Code_Point := 0;
   RI_Last  : UCD.Code_Point := 0;
   --  First and last character of the characters with Regional_Indicator
   --  property.

   -----------
   -- Build --
   -----------

   procedure Build is
      GC_Property   : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("gc");
      GCB_Property  : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("GCB");
      WB_Property   : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("WB");
      InCB_Property : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("InCB");
      EA_Property   : constant not null UCD.Properties.Property_Access :=
        UCD.Properties.Resolve ("ea");

   begin
      Put ("   ... core properties");

      GC_Enumeration.Initialize (GC_Property);
      GCB_Enumeration.Initialize (GCB_Property);
      WB_Enumeration.Initialize (WB_Property);
      InCB_Enumeration.Initialize (InCB_Property, "None");
      EA_Enumeration.Initialize (EA_Property, "N");

      Database.Initialize (8);

      declare
         use type UCD.Properties.Property_Value_Access;
         use type UCD.Code_Point;

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
         Emoji_Property   : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("Emoji");
         Emoji_Y          :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (Emoji_Property, "Y");
         EPres_Property   : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("EPres");
         EPres_Y          :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (EPres_Property, "Y");
         EBase_Property   : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("EBase");
         EBase_Y          :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (EBase_Property, "Y");
         EMod_Property    : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("EMod");
         EMod_Y           :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (EMod_Property, "Y");
         RI_Property      : constant not null UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("RI");
         RI_Y             :
           constant not null UCD.Properties.Property_Value_Access :=
             UCD.Properties.Resolve (RI_Property, "Y");

      begin
         for Code in UCD.Code_Point loop
            Database.Set_GC
              (Code, Unsigned_5 (GC_Enumeration.Representation (Code)));

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
              (Code, Unsigned_4 (GCB_Enumeration.Representation (Code)));

            Database.Set_WB
              (Code, Unsigned_5 (WB_Enumeration.Representation (Code)));

            Database.Set_InCB
              (Code, Unsigned_2 (InCB_Enumeration.Representation (Code)));

            Database.Set_EA
              (Code, Unsigned_3 (EA_Enumeration.Representation (Code)));

            Database.Set_Emoji
              (Code, UCD.Characters.Get (Code, Emoji_Property) = Emoji_Y);
            Database.Set_EBase
              (Code, UCD.Characters.Get (Code, EBase_Property) = EBase_Y);
            Database.Set_EMod
              (Code, UCD.Characters.Get (Code, EMod_Property) = EMod_Y);
            Database.Set_EPres
              (Code, UCD.Characters.Get (Code, EPres_Property) = EPres_Y);

            --  Regional_Indicator is a property for continuous range of
            --  the characters, subtype is generated to check value of
            --  this property.

            if UCD.Characters.Get (Code, RI_Property) = RI_Y then
               if RI_First = 0 then
                  RI_First := Code;

               else
                  if Code /= RI_Last + 1 then
                     --  Interval should be continuous.

                     raise Program_Error;
                  end if;
               end if;

               RI_Last  := Code;
            end if;
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

      --  GC, OLower, and OUpper are used for character classification in
      --  public API, and put into the first byte.
      --
      --  GCB, ExtPict, and InCB are used by grapheme cluster iterator and
      --  put into second byte of the record.
      --
      --  WB & ExtPict (from second byte) are used by word iterator and put
      --  into third byte.
      --
      --  EA, Emoji, EBase, EMod, EPres are used by terminal cells length
      --  calculation and put into fourth byte.

      --    31  |  30  |  29  |  28  |  27  |  26  |  25  |  24
      --    --  | EPres| EMod | EBase| Emoji|         EA
      --
      --    23  |  22  |  21  |  20  |  19  |  18  |  17  |  16
      --    --     --     --  |                WB
      --
      --    15  |  14  |  13  |  12  |  11  |  10  |  09  |  08
      --    --  | EPic |     InCB    |            GCB
      --
      --    07  |  06  |  05  |  04  |  03  |  02  |  01  |  00
      --    --  | OUpp | OLow |                CG

      type Core_Data_Record is record
         GC             : Gen_UCD.Unsigned_5 := 0;
         OLower         : Gen_UCD.Unsigned_1 := 0;
         OUpper         : Gen_UCD.Unsigned_1 := 0;
         Reserved_7_7   : Gen_UCD.Unsigned_1 := 0;
         GCB            : Gen_UCD.Unsigned_4 := 0;
         InCB           : Gen_UCD.Unsigned_2 := 0;
         ExtPict        : Gen_UCD.Unsigned_1 := 0;
         Reserved_15_15 : Gen_UCD.Unsigned_1 := 0;
         WB             : Gen_UCD.Unsigned_5 := 0;
         Reserved_21_23 : Gen_UCD.Unsigned_3 := 0;
         EA             : Gen_UCD.Unsigned_3 := 0;
         Emoji          : Gen_UCD.Unsigned_1 := 0;
         EBase          : Gen_UCD.Unsigned_1 := 0;
         EMod           : Gen_UCD.Unsigned_1 := 0;
         EPres          : Gen_UCD.Unsigned_1 := 0;
         Reserved_31_31 : Gen_UCD.Unsigned_1 := 0;
      end record;
      for Core_Data_Record'Size use 32;
      for Core_Data_Record use record
         GC             at 0 range 0 .. 4;
         OLower         at 0 range 5 .. 5;
         OUpper         at 0 range 6 .. 6;
         Reserved_7_7   at 0 range 7 .. 7;
         GCB            at 0 range 8 .. 11;
         InCB           at 0 range 12 .. 13;
         ExtPict        at 0 range 14 .. 14;
         Reserved_15_15 at 0 range 15 .. 15;
         WB             at 0 range 16 .. 20;
         Reserved_21_23 at 0 range 21 .. 23;
         EA             at 0 range 24 .. 26;
         Emoji          at 0 range 27 .. 27;
         EBase          at 0 range 28 .. 28;
         EMod           at 0 range 29 .. 29;
         EPres          at 0 range 30 .. 30;
         Reserved_31_31 at 0 range 31 .. 31;
      end record;

      type Core_Data_Array is
        array (Gen_UCD.Unsigned_32 range <>) of Core_Data_Record;

      type Core_Data_Array_Access is access all Core_Data_Array;

      type Unsigned_32_Array is
        array (Gen_UCD.Unsigned_32 range <>) of Gen_UCD.Unsigned_32;

      type Unsigned_32_Array_Access is access all Unsigned_32_Array;

      Raw         : Core_Data_Array_Access;

      Compressed_Block_Size : Gen_UCD.Unsigned_32;
      Compressed_Data       : Core_Data_Array_Access;
      Compressed_Data_Last  : Gen_UCD.Unsigned_32;
      Index_Data            : Unsigned_32_Array_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Core_Data_Array, Core_Data_Array_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Unsigned_32_Array, Unsigned_32_Array_Access);

      procedure Compress
        (Block_Size : Gen_UCD.Unsigned_32;
         Done       : in out Boolean);

      function Memory_Consumption
        (Compressed_Data_Last : Gen_UCD.Unsigned_32;
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
        (Block_Size : Gen_UCD.Unsigned_32;
         Done       : in out Boolean)
      is

         function Is_Equal
           (Raw_Block  : Gen_UCD.Unsigned_32;
            Compressed : Gen_UCD.Unsigned_32) return Boolean;

         --------------
         -- Is_Equal --
         --------------

         function Is_Equal
           (Raw_Block  : Gen_UCD.Unsigned_32;
            Compressed : Gen_UCD.Unsigned_32) return Boolean is
         begin
            return
              Raw (Raw_Block * Block_Size .. (Raw_Block + 1) * Block_Size - 1)
                = Compressed_Data (Compressed .. Compressed + Block_Size - 1);
         end Is_Equal;

         Previous_Compressed_Block_Size : constant Gen_UCD.Unsigned_32 :=
           Compressed_Block_Size;
         Previous_Compressed_Data       : Core_Data_Array_Access :=
           Compressed_Data;
         Previous_Compressed_Data_Last  : constant Gen_UCD.Unsigned_32 :=
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
        (Index : Natural) return Gen_UCD.Unsigned_32
      is
         function To_Unsigned_32 is
           new Ada.Unchecked_Conversion
                 (Core_Data_Record, Gen_UCD.Unsigned_32);

      begin
         return To_Unsigned_32 (Compressed_Data (Gen_UCD.Unsigned_32 (Index)));
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
         return Natural (Index_Data (Gen_UCD.Unsigned_32 (Index)));
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
             (0 .. Gen_UCD.Unsigned_32 (UCD.Code_Point'Last));
      end Initialize;

      ------------------------
      -- Memory_Consumption --
      ------------------------

      function Memory_Consumption
        (Compressed_Data_Last : Gen_UCD.Unsigned_32;
         Index_Data           : Unsigned_32_Array) return Integer is
      begin
         return
           Integer
             ((Compressed_Data_Last + 1) * 4
              + (if Compressed_Data_Last
                   <= Gen_UCD.Unsigned_32 (Gen_UCD.Unsigned_16'Last)
                then Index_Data'Length * 2
                else Index_Data'Length * 4));
      end Memory_Consumption;

      ------------
      -- Set_EA --
      ------------

      procedure Set_EA (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_3) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).EA := To;
      end Set_EA;

      ---------------
      -- Set_EBase --
      ---------------

      procedure Set_EBase (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).EBase := Boolean'Pos (To);
      end Set_EBase;

      --------------
      -- Set_EMod --
      --------------

      procedure Set_EMod (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).EMod := Boolean'Pos (To);
      end Set_EMod;

      ---------------
      -- Set_Emoji --
      ---------------

      procedure Set_Emoji (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).Emoji := Boolean'Pos (To);
      end Set_Emoji;

      ---------------
      -- Set_EPres --
      ---------------

      procedure Set_EPres (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).EPres := Boolean'Pos (To);
      end Set_EPres;

      -----------------
      -- Set_ExtPict --
      -----------------

      procedure Set_ExtPict (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).ExtPict := Boolean'Pos (To);
      end Set_ExtPict;

      ------------
      -- Set_GC --
      ------------

      procedure Set_GC (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_5) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).GC := To;
      end Set_GC;

      -------------
      -- Set_GCB --
      -------------

      procedure Set_GCB
        (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_4) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).GCB := To;
      end Set_GCB;

      --------------
      -- Set_InCB --
      --------------

      procedure Set_InCB (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_2) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).InCB := To;
      end Set_InCB;

      ----------------
      -- Set_OLower --
      ----------------

      procedure Set_OLower (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).OLower := Boolean'Pos (To);
      end Set_OLower;

      ----------------
      -- Set_OUpper --
      ----------------

      procedure Set_OUpper (Code : UCD.Code_Point; To : Boolean) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).OUpper := Boolean'Pos (To);
      end Set_OUpper;

      ------------
      -- Set_WB --
      ------------

      procedure Set_WB (Code : UCD.Code_Point; To : Gen_UCD.Unsigned_5) is
      begin
         Raw (Gen_UCD.Unsigned_32 (Code)).WB := To;
      end Set_WB;

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

      Put_Line (File, "pragma Ada_2022;");
      Put_Line (File, "pragma Restrictions (No_Elaboration_Code);");
      New_Line (File);

      Put_Line (File, "with Interfaces;");
      New_Line (File);
      Put_Line (File, "with VSS.Unicode;");
      New_Line (File);
      Put_Line (File, "package VSS.Implementation.UCD_Core is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      New_Line (File);

      --  Generate GC_Values type

      GC_Enumeration.Generate_Type_Declaration (File);

      --  Generate GCB_Values type

      GCB_Enumeration.Generate_Type_Declaration (File);

      --  Generate WB_Values type

      WB_Enumeration.Generate_Type_Declaration (File);

      --  Generate InCB_Values type

      InCB_Enumeration.Generate_Type_Declaration (File);

      --  Generate EA_Values type

      EA_Enumeration.Generate_Type_Declaration (File);

      --  Generate declaration of the Regional_Indicator_Range subtype.

      declare
         First_Image : Wide_Wide_String (1 .. 11);
         Last_Image  : Wide_Wide_String (1 .. 11);

      begin
         Put (First_Image, Integer (RI_First), 16);
         Put (Last_Image, Integer (RI_Last), 16);

         Put_Line
           (File,
            "   subtype Regional_Indicator_Range is VSS.Unicode.Code_Point");
         Put_Line
           (File,
            "     range "
            & Trim (First_Image, Both)
            & " .. "
            & Trim (Last_Image, Both)
            & ";");
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
         Put_Line (File, "      InCB    : INCB_Values;");
         Put_Line (File, "      ExtPict : Boolean;");
         Put_Line (File, "      WB      : WB_Values;");
         Put_Line (File, "      EA      : EA_Values;");
         Put_Line (File, "      Emoji   : Boolean;");
         Put_Line (File, "      EBase   : Boolean;");
         Put_Line (File, "      EMod    : Boolean;");
         Put_Line (File, "      EPres   : Boolean;");
         Put_Line (File, "   end record;");
         Put_Line (File, "   for Core_Data_Record'Size use 32;");
         Put_Line (File, "   for Core_Data_Record use record");
         Put_Line (File, "      GC      at 0 range 0 .. 4;");
         Put_Line (File, "      OLower  at 0 range 5 .. 5;");
         Put_Line (File, "      OUpper  at 0 range 6 .. 6;");
         Put_Line (File, "      GCB     at 0 range 8 .. 11;");
         Put_Line (File, "      InCB    at 0 range 12 .. 13;");
         Put_Line (File, "      ExtPict at 0 range 14 .. 14;");
         Put_Line (File, "      WB      at 0 range 16 .. 20;");
         Put_Line (File, "      EA      at 0 range 24 .. 26;");
         Put_Line (File, "      Emoji   at 0 range 27 .. 27;");
         Put_Line (File, "      EBase   at 0 range 28 .. 28;");
         Put_Line (File, "      EMod    at 0 range 29 .. 29;");
         Put_Line (File, "      EPres   at 0 range 30 .. 30;");
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
            & " array (Core_Offset) of Interfaces.Unsigned_32;");
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
               Put (File, "     [");

            elsif J mod 8 = 0 then
               Put_Line (File, ",");
               Put (File, "      ");

            else
               Put (File, ", ");
            end if;

            Put (Image, Database.Index_Table_Element (J));
            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, "];");
         New_Line (File);
      end;

      --  Generate data table.

      declare
         Image : Wide_Wide_String (1 .. 13);

      begin
         Put_Line
           (File,
            "   Core_Data_Raw_Table : constant Core_Data_Raw_Array :=");

         for J in 0 .. Database.Data_Index_Last loop
            Put (Image, Integer (Database.Data_Table_Element (J)), 16);

            if J = 0 then
               Put (File, "     [");

            elsif J mod 6 = 0 then
               Put_Line (File, ",");
               Put (File, "      ");

            else
               Put (File, ", ");
            end if;

            Put (File, Trim (Image, Both));
         end loop;

         Put_Line (File, "];");
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

end Gen_UCD.Core_Properties;
