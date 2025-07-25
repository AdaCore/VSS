--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Character_Codes;
with VSS.Implementation.UCD_Core;

with VSS.Strings.Cursors.Markers;
pragma Unreferenced (VSS.Strings.Cursors.Markers);
--  XXX GNAT 20210710: crash without clause above.

package body VSS.Strings.Cursors.Iterators.Grapheme_Clusters is

   use type VSS.Implementation.Referrers.Magic_String_Access;
   use type VSS.Implementation.Strings.Character_Offset;
   use all type VSS.Implementation.UCD_Core.GCB_Values;
   use all type VSS.Implementation.UCD_Core.INCB_Values;

   procedure Lookup_Grapheme_Cluster_Boundaries
     (Self     : in out Grapheme_Cluster_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor);
   --  Lookup for grapheme cluster boundaries around given position and setup
   --  iterator to point to found segment.

   function Extract_Core_Data
     (Code : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Core.Core_Data_Record;
   --  Return core data record for the given character.

   function Apply_RI
     (Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Left : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string backward to check whether Rules GB12, GB13 should be
   --  applied.

   function Apply_ExtPict
     (Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Left : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string backward to check whether Rule GB11 should be applied.

   function Apply_InCB
     (Text      : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Left      : VSS.Implementation.Strings.Cursor;
      Is_Linker : Boolean) return Boolean;
   --  Scan string backward to check whether Rule GB9c should be applied.

   type GCB_Action is (Break, No_Break, Unspecified);

   --  The table below encodes segmentation rules that depend only on the
   --  value of the GCB property.

   Forward_GCB_Rules : constant array
     (VSS.Implementation.UCD_Core.GCB_Values,
      VSS.Implementation.UCD_Core.GCB_Values) of GCB_Action :=
     [GCB_CN  => [others => Break],                     --  Rule GB4
      GCB_CR  =>
        [GCB_LF => No_Break,                            --  Rule GB3
         others => Break],                              --  Rule GB4
      GCB_L   =>
        [GCB_CN | GCB_CR | GCB_LF         => Break,     --  Rule GB5
         GCB_L | GCB_V | GCB_LV | GCB_LVT => No_Break,  --  Rule GB6
         GCB_EX | GCB_ZWJ                 => No_Break,  --  Rule GB9
         GCB_SM                           => No_Break,  --  Rule GB9a
         others                           => Unspecified],
      GCB_LF  => [others => Break],                     --  Rule GB4
      GCB_LV  =>
        [GCB_CN | GCB_CR | GCB_LF => Break,             --  Rule GB5
         GCB_V | GCB_T            => No_Break,          --  Rule GB7
         GCB_EX | GCB_ZWJ         => No_Break,          --  Rule GB9
         GCB_SM                   => No_Break,          --  Rule GB9a
         others                   => Unspecified],
      GCB_LVT =>
        [GCB_CN | GCB_CR | GCB_LF => Break,             --  Rule GB5
         GCB_T                    => No_Break,          --  Rule GB8
         GCB_EX | GCB_ZWJ         => No_Break,          --  Rule GB9
         GCB_SM                   => No_Break,          --  Rule GB9a
         others                   => Unspecified],
      GCB_PP  =>
        [GCB_CN | GCB_CR | GCB_LF => Break,             --  Rule GB5
         GCB_EX | GCB_ZWJ         => No_Break,          --  Rule GB9
         GCB_SM                   => No_Break,          --  Rule GB9a
         others                   => No_Break],         --  Rule GB9b
      GCB_T =>
        [GCB_CN | GCB_CR | GCB_LF => Break,             --  Rule GB5
         GCB_T                    => No_Break,          --  Rule GB8
         GCB_EX | GCB_ZWJ         => No_Break,          --  Rule GB9
         GCB_SM                   => No_Break,          --  Rule GB9a
         others                   => Unspecified],
      GCB_V   =>
        [GCB_CN | GCB_CR | GCB_LF => Break,             --  Rule GB5
         GCB_V | GCB_T            => No_Break,          --  Rule GB7
         GCB_EX | GCB_ZWJ         => No_Break,          --  Rule GB9
         GCB_SM                   => No_Break,          --  Rule GB9a
         others                   => Unspecified],
      GCB_EX | GCB_RI | GCB_SM | GCB_XX | GCB_ZWJ =>
        [GCB_CN | GCB_CR | GCB_LF => Break,             --  Rule GB5
         GCB_EX | GCB_ZWJ         => No_Break,          --  Rule GB9
         GCB_SM                   => No_Break,          --  Rule GB9a
         others                   => Unspecified]];

   -------------------
   -- Apply_ExtPict --
   -------------------

   function Apply_ExtPict
     (Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Left : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : aliased VSS.Implementation.Strings.Cursor := Left;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not VSS.Implementation.UTF8_Strings.Backward (Text, Position) then
            return False;
         end if;

         Properties :=
           Extract_Core_Data
             (VSS.Implementation.UTF8_Strings.Element (Text, Position));

         if Properties.GCB = GCB_EX then
            null;

         elsif Properties.ExtPict then
            return True;

         else
            return False;
         end if;
      end loop;
   end Apply_ExtPict;

   ----------------
   -- Apply_InCB --
   ----------------

   function Apply_InCB
     (Text      : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Left      : VSS.Implementation.Strings.Cursor;
      Is_Linker : Boolean) return Boolean
   is
      Position   : aliased VSS.Implementation.Strings.Cursor := Left;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Has_Linker : Boolean := Is_Linker;

   begin
      loop
         if not VSS.Implementation.UTF8_Strings.Backward (Text, Position) then
            return False;
         end if;

         Properties :=
           Extract_Core_Data
             (VSS.Implementation.UTF8_Strings.Element (Text, Position));

         case Properties.InCB is
            when INCB_Linker =>
               Has_Linker := True;

            when INCB_Extend =>
               null;

            when INCB_Consonant =>
               return Has_Linker;

            when INCB_None =>
               return False;
         end case;
      end loop;
   end Apply_InCB;

   --------------
   -- Apply_RI --
   --------------

   function Apply_RI
     (Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Left : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position : aliased VSS.Implementation.Strings.Cursor := Left;
      Count    : Natural := 0;

   begin
      loop
         if not VSS.Implementation.UTF8_Strings.Backward (Text, Position) then
            return Count mod 2 = 0;
         end if;

         if Extract_Core_Data
           (VSS.Implementation.UTF8_Strings.Element (Text, Position)).GCB
              = GCB_RI
         then
            Count := Count + 1;

         else
            return Count mod 2 = 0;
         end if;
      end loop;
   end Apply_RI;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self : in out Grapheme_Cluster_Iterator) return Boolean is
   begin
      if Self.Owner = null then
         --  Uninitialized iterator.

         return False;
      end if;

      declare
         Text             : VSS.Implementation.UTF8_Strings.UTF8_String_Data
           renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
         Right            : VSS.Implementation.Strings.Cursor;
         Right_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
         Left             : aliased VSS.Implementation.Strings.Cursor;
         Left_Properties  : VSS.Implementation.UCD_Core.Core_Data_Record;
         Success          : Boolean;
         Done             : Boolean := False;

      begin
         Self.Last_Position := Self.First_Position;
         Success :=
           VSS.Implementation.UTF8_Strings.Backward (Text, Self.Last_Position);

         if not Success then
            --  Start of the line has been reached.

            Self.First_Position := Self.Last_Position;

            return False;

         else
            Left            := Self.Last_Position;
            Left_Properties :=
              Extract_Core_Data
                (VSS.Implementation.UTF8_Strings.Element (Text, Left));

            loop
               Right            := Left;
               Right_Properties := Left_Properties;

               Success :=
                 VSS.Implementation.UTF8_Strings.Backward (Text, Left);

               if not Success then
                  --  Start of the string has been reached.

                  Self.First_Position := Right;

                  return True;

               else
                  Left_Properties :=
                    Extract_Core_Data
                      (VSS.Implementation.UTF8_Strings.Element (Text, Left));

                  if Left_Properties.GCB = GCB_CR
                    and Right_Properties.GCB = GCB_LF
                  then
                     --  Rule GB3.

                     null;

                  elsif Left_Properties.GCB in GCB_CN | GCB_CR | GCB_LF then
                     --  Rule GB4.

                     Done := True;

                  elsif Right_Properties.GCB in  GCB_CN | GCB_CR | GCB_LF then
                     --  Rule GB5.

                     Done := True;

                  elsif Left_Properties.GCB = GCB_L
                    and Right_Properties.GCB
                          in GCB_L | GCB_V | GCB_LV | GCB_LVT
                  then
                     --  Rule GB6.

                     null;

                  elsif Left_Properties.GCB in GCB_LV | GCB_V
                    and Right_Properties.GCB in GCB_V | GCB_T
                  then
                     --  Rule GB7.

                     null;

                  elsif Left_Properties.GCB in GCB_LVT | GCB_T
                    and Right_Properties.GCB in GCB_T
                  then
                     --  Rule GB8.

                     null;

                  elsif Right_Properties.GCB in GCB_EX | GCB_ZWJ then
                     --  Rule GB9.

                     null;

                  elsif Right_Properties.GCB = GCB_SM then
                     --  Rule 9a.

                     null;

                  elsif Left_Properties.GCB = GCB_PP then
                     --  Rule 9b.

                     null;

                  elsif Left_Properties.InCB in INCB_Linker | INCB_Extend
                    and then Right_Properties.InCB = INCB_Consonant
                    and then Apply_InCB
                      (Text, Left, Left_Properties.InCB = INCB_Linker)
                  then
                     --  Rule 9c.

                     null;

                  elsif Left_Properties.GCB = GCB_ZWJ
                    and then Right_Properties.ExtPict
                    and then Apply_ExtPict (Text, Left)
                  then
                     --  Rule 11.

                     null;

                  elsif Left_Properties.GCB = GCB_RI
                    and then Right_Properties.GCB = GCB_RI
                    and then Apply_RI (Text, Left)
                  then
                     --  Rule GB12.
                     --  Rule GB13.

                     null;

                  else
                     --  Rule GB999.

                     Done := True;
                  end if;

                  if Done then
                     Self.First_Position := Right;

                     return True;
                  end if;
               end if;
            end loop;
         end if;
      end;
   end Backward;

   -------------------
   -- Display_Width --
   -------------------

   function Display_Width
     (Self : Grapheme_Cluster_Iterator'Class)
      return VSS.Strings.Display_Cell_Count is
   begin
      if Self.Owner = null then
         --  Uninitialized iterator.

         return 0;
      end if;

      declare
         use all type VSS.Implementation.UCD_Core.EA_Values;

         Text       : VSS.Implementation.UTF8_Strings.UTF8_String_Data
           renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
         Position   : aliased VSS.Implementation.Strings.Cursor;
         Code       : VSS.Unicode.Code_Point'Base;
         Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
         Success    : Boolean with Unreferenced;

      begin
         if Self.First_Position.Index not in 1 .. Text.Length then
            --  Iterator doesn't point to any grapheme cluster.

            return 0;
         end if;

         --  Lookup for the wide/fullwidth character in the grapheme cluster.

         Position := Self.First_Position;
         Code     := VSS.Implementation.UTF8_Strings.Element (Text, Position);

         loop
            Properties := Extract_Core_Data (Code);

            if Properties.EA in EA_W | EA_F then
               return 2;
            end if;

            exit when
            not VSS.Implementation.UTF8_Strings.Forward_Element
              (Text, Position, Code);
            exit when Position.Index > Self.Last_Position.Index;
         end loop;

         --  Emoji_Presentation characters has wide width, and has
         --  been processed above. However, some combinations of
         --  non-Emoji_Presentation characters forms emoji, thus occupy
         --  two cell.

         if not Self.Is_Emoji then
            return 1;
         end if;

         if Self.First_Position.Index = Self.Last_Position.Index then
            --  Single emoji character without Emoji_Presenataion property has
            --  default text representation and occupy single cell.

            Position   := Self.First_Position;
            Code       :=
              VSS.Implementation.UTF8_Strings.Element (Text, Position);
            Properties := Extract_Core_Data (Code);

            if not Properties.EPres then
               return 1;
            end if;
         end if;

         return 2;
      end;
   end Display_Width;

   -----------------------
   -- Extract_Core_Data --
   -----------------------

   function Extract_Core_Data
     (Code : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Core.Core_Data_Record
   is
      use type VSS.Implementation.UCD_Core.Core_Offset;
      use type VSS.Unicode.Code_Point;

      Block : constant VSS.Implementation.UCD_Core.Core_Index :=
        VSS.Implementation.UCD_Core.Core_Index
          (Code / VSS.Implementation.UCD_Core.Block_Size);
      Offset : constant VSS.Implementation.UCD_Core.Core_Offset :=
        VSS.Implementation.UCD_Core.Core_Offset
          (Code mod VSS.Implementation.UCD_Core.Block_Size);

   begin
      return
        VSS.Implementation.UCD_Core.Core_Data_Table
          (VSS.Implementation.UCD_Core.Core_Index_Table (Block) + Offset);
   end Extract_Core_Data;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self : in out Grapheme_Cluster_Iterator) return Boolean
   is
   begin
      if Self.Owner = null then
         --  Uninitialized iterator.

         return False;
      end if;

      declare
         Text             : VSS.Implementation.UTF8_Strings.UTF8_String_Data
           renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
         Left             : VSS.Implementation.Strings.Cursor;
         Left_Properties  : VSS.Implementation.UCD_Core.Core_Data_Record;
         Right            : aliased VSS.Implementation.Strings.Cursor;
         Right_Code       : VSS.Unicode.Code_Point'Base;
         Right_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
         Success          : Boolean;
         Done             : Boolean := False;

      begin
         Self.First_Position := Self.Last_Position;
         Success :=
           VSS.Implementation.UTF8_Strings.Forward_Element
             (Text, Self.First_Position, Right_Code);

         if not Success then
            --  End of the string has been reached.
            --  XXX Should Last_Position be set to After_Last_Character?

            return False;
         end if;

         Right            := Self.First_Position;
         Right_Properties := Extract_Core_Data (Right_Code);

         loop
            Left            := Right;
            Left_Properties := Right_Properties;

            Success :=
              VSS.Implementation.UTF8_Strings.Forward_Element
                (Text, Right, Right_Code);

            if not Success then
               --  End of line has been reached
               --  Rule GB2

               Self.Last_Position := Left;

               return True;
            end if;

            Right_Properties := Extract_Core_Data (Right_Code);

            case Forward_GCB_Rules (Left_Properties.GCB, Right_Properties.GCB)
            is
               when Break =>
                  Done := True;

               when No_Break =>
                  null;

               when Unspecified =>
                  if Left_Properties.InCB in INCB_Linker | INCB_Extend
                    and then Right_Properties.InCB = INCB_Consonant
                    and then Apply_InCB
                      (Text, Left, Left_Properties.InCB = INCB_Linker)
                  then
                     --  Rule GB9c.

                     null;

                  elsif Left_Properties.GCB = GCB_ZWJ
                    and then Right_Properties.ExtPict
                    and then Apply_ExtPict (Text, Left)
                  then
                     --  Rule GB11.

                     null;

                  elsif Left_Properties.GCB = GCB_RI
                    and then Right_Properties.GCB = GCB_RI
                    and then Apply_RI (Text, Left)
                  then
                     --  Rule GB12.
                     --  Rule GB13.

                     null;

                  else
                     Done := True;
                  end if;
            end case;

            if Done then
               Self.Last_Position := Left;

               return True;
            end if;
         end loop;
      end;
   end Forward;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Grapheme_Cluster_Iterator) return Boolean
   is
      Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner = null then
         --  Uninitialized iterator.

         return False;
      end if;

      return Self.First_Position.Index in 1 .. Text.Length;
   end Has_Element;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Grapheme_Cluster_Iterator) is
   begin
      Abstract_Segment_Iterator (Self).Invalidate;
   end Invalidate;

   --------------
   -- Is_Emoji --
   --------------

   function Is_Emoji
     (Self : Grapheme_Cluster_Iterator'Class) return Boolean
   is
      use type VSS.Unicode.Code_Point;

      type Emoji_State is record
         Is_Emoji                       : Boolean;
         Is_Emoji_Presentation_Sequence : Boolean;
         Is_Emoji_Modifier_Sequence     : Boolean;
         Is_Emoji_Keycap_Sequence       : Boolean;
         Is_Emoji_Flag_Sequence         : Boolean;
      end record;

      Text       : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Position   : aliased VSS.Implementation.Strings.Cursor :=
        Self.First_Position;
      Code       : VSS.Unicode.Code_Point'Base;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      State      : Emoji_State := (others => False);

      subtype Tag_Spec_Range is VSS.Unicode.Code_Point
        range 16#E0020# .. 16#E007E#;

      Tag_End : constant := 16#E007F#;

   begin
      if Self.Owner = null then
         --  Uninitialized iterator.

         return False;
      end if;

      if Self.First_Position.Index not in 1 .. Text.Length then
         --  Iterator doesn't point to grapheme cluster.

         return False;
      end if;

      Code := VSS.Implementation.UTF8_Strings.Element (Text, Position);

      --  Outer loop parses `emoji_zwj_element` expression.

      loop
         --  First character of the `emoji_zwj_element` might be Emoji, '#',
         --  '*', '0'..'9', `Emoji_Modifier_Base' or `Regional_Indicator`.
         --
         --  As of Unicode 15.1, characters '#', '*', '0'..'9',
         --  `Emoji_Modifier_Base` and `Regional_Indicator` has
         --  `Emoji` property.
         --
         --  Only `Emoji` complete emoji_zwj_element

         Properties := Extract_Core_Data (Code);

         if not Properties.Emoji then
            --  Non-Emoji character can't start emoji seqeunce.

            return False;
         end if;

         State.Is_Emoji                       := Properties.Emoji;
         State.Is_Emoji_Presentation_Sequence := Properties.Emoji;
         State.Is_Emoji_Modifier_Sequence     := Properties.EBase;
         State.Is_Emoji_Keycap_Sequence       :=
           Code in VSS.Implementation.Character_Codes.Digit_Zero
                     .. VSS.Implementation.Character_Codes.Digit_Nine
                   | VSS.Implementation.Character_Codes.Number_Sign
                   | VSS.Implementation.Character_Codes.Asterisk;
         State.Is_Emoji_Flag_Sequence         :=
           Code in VSS.Implementation.UCD_Core.Regional_Indicator_Range;

         --  Second character of the `emoji_zwj_element` might be U+FF0F,
         --  `Emoji_Modifier`, or `Regional_Indicator`.
         --
         --  This charater can be `tag_spec` or ZWJ too.

         exit when
           not VSS.Implementation.UTF8_Strings.Forward_Element
                 (Text, Position, Code);
         exit when Position.Index > Self.Last_Position.Index;

         Properties := Extract_Core_Data (Code);

         case Code is
            when VSS.Implementation.Character_Codes.Zero_Width_Joiner =>
               goto ZWJ;

            when Tag_Spec_Range =>
               goto TAG;

            when VSS.Implementation.Character_Codes.Variation_Selector_16 =>
               if State.Is_Emoji_Keycap_Sequence then
                  --  Incomplete `emoji_keycap_sequence`. It match
                  --  `emoji_presentation_sequence` too.

                  null;
                  --  State.Is_Emoji := False;

               elsif State.Is_Emoji_Presentation_Sequence then
                  --  Complete `emoji_presentation_sequence`

                  null;

               else
                  return False;
               end if;

            when VSS.Implementation.UCD_Core.Regional_Indicator_Range =>
               if not State.Is_Emoji_Flag_Sequence then
                  return False;
               end if;

            when others =>
               if not State.Is_Emoji_Modifier_Sequence
                    or not Properties.EMod
               then
                  return False;
               end if;

               State.Is_Emoji_Keycap_Sequence := False;
         end case;

         --  Third character of the `emoji_zwj_element` might be U+20E3,
         --  `tag_spec` or ZWJ.

         exit when
           not VSS.Implementation.UTF8_Strings.Forward_Element
                 (Text, Position, Code);
         exit when Position.Index > Self.Last_Position.Index;

         case Code is
            when Tag_Spec_Range =>
               if State.Is_Emoji_Flag_Sequence then
                  --  `tag_spec` can't follow `emoji_flag_sequence`

                  return False;
               end if;

               goto TAG;

            when VSS.Implementation.Character_Codes.Zero_Width_Joiner =>
               goto ZWJ;

            when VSS.Implementation.Character_Codes
                   .Combining_Enclosing_Keycap
            =>
               if not State.Is_Emoji_Keycap_Sequence then
                  return False;
               end if;

            when others =>
               return False;
         end case;

         --  Forth element, it can follow `emoji_keycap_sequence` only, and
         --  might be ZWJ only

         exit when
           not VSS.Implementation.UTF8_Strings.Forward_Element
                 (Text, Position, Code);
         exit when Position.Index > Self.Last_Position.Index;

         if Code = VSS.Implementation.Character_Codes.Zero_Width_Joiner then
            goto ZWJ;
         end if;

         return False;

         <<TAG>>

         pragma Assert (Code in Tag_Spec_Range);

         State.Is_Emoji := False;

         loop
            exit when
              not VSS.Implementation.UTF8_Strings.Forward_Element
                    (Text, Position, Code);
            exit when Position.Index > Self.Last_Position.Index;

            case Code is
               when Tag_Spec_Range =>
                  null;

               when Tag_End =>
                  State.Is_Emoji := True;

                  exit;

               when others =>
                  return False;
            end case;
         end loop;

         exit when
           not VSS.Implementation.UTF8_Strings.Forward_Element
                 (Text, Position, Code);
         exit when Position.Index > Self.Last_Position.Index;

         if Code = VSS.Implementation.Character_Codes.Zero_Width_Joiner then
            goto ZWJ;

         else
            raise Program_Error;
         end if;

         <<ZWJ>>

         if not State.Is_Emoji then
            return False;
         end if;

         State.Is_Emoji := False;

         exit when
           not VSS.Implementation.UTF8_Strings.Forward_Element
                 (Text, Position, Code);
         exit when Position.Index > Self.Last_Position.Index;
      end loop;

      return State.Is_Emoji;
   end Is_Emoji;

   ----------------------------------------
   -- Lookup_Grapheme_Cluster_Boundaries --
   ----------------------------------------

   procedure Lookup_Grapheme_Cluster_Boundaries
     (Self     : in out Grapheme_Cluster_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor)
   is
      Text    : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Success : Boolean with Unreferenced;

   begin
      if Position.Index = 0 then
         --  Before first character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index > Text.Length then
         --  After last character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index = 1 then
         --  First character of the string, it starts first grapheme cluster.

         VSS.Implementation.UTF8_Strings.Before_First_Character
           (Text, Self.First_Position);
         VSS.Implementation.UTF8_Strings.Before_First_Character
           (Text, Self.Last_Position);
         Success := Self.Forward;

      elsif Position.Index = Text.Length then
         --  Last character of the string, it ends last grapheme cluster.

         Self.Last_Position  := Position;
         Self.First_Position := Position;
         Success :=
           VSS.Implementation.UTF8_Strings.Forward (Text, Self.First_Position);
         Success := Self.Backward;

      else
         raise Program_Error;
      end if;
   end Lookup_Grapheme_Cluster_Boundaries;

   --------------------
   -- Set_After_Last --
   --------------------

   procedure Set_After_Last
     (Self : in out Grapheme_Cluster_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Position : VSS.Implementation.Strings.Cursor;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      VSS.Implementation.UTF8_Strings.After_Last_Character (On.Data, Position);
      Self.Lookup_Grapheme_Cluster_Boundaries (Position);
   end Set_After_Last;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (Self     : in out Grapheme_Cluster_Iterator;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
   is
      Cursor_Owner    : VSS.Implementation.Referrers.Magic_String_Access;
      Cursor_Position : VSS.Implementation.Strings.Cursor;

   begin
      Get_Owner_And_Position (Position, Cursor_Owner, Cursor_Position);

      Self.Reconnect (Cursor_Owner);

      if Self.Owner /= null then
         Self.Lookup_Grapheme_Cluster_Boundaries (Cursor_Position);

      else
         Self.Invalidate;
      end if;
   end Set_At;

   ------------------
   -- Set_At_First --
   ------------------

   procedure Set_At_First
     (Self : in out Grapheme_Cluster_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      VSS.Implementation.UTF8_Strings.Before_First_Character
        (On.Data, Position);
      Dummy := VSS.Implementation.UTF8_Strings.Forward (On.Data, Position);
      Self.Lookup_Grapheme_Cluster_Boundaries (Position);
   end Set_At_First;

   -----------------
   -- Set_At_Last --
   -----------------

   procedure Set_At_Last
     (Self : in out Grapheme_Cluster_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      VSS.Implementation.UTF8_Strings.After_Last_Character (On.Data, Position);
      Dummy := VSS.Implementation.UTF8_Strings.Backward (On.Data, Position);
      Self.Lookup_Grapheme_Cluster_Boundaries (Position);
   end Set_At_Last;

   ----------------------
   -- Set_Before_First --
   ----------------------

   procedure Set_Before_First
     (Self : in out Grapheme_Cluster_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Position : VSS.Implementation.Strings.Cursor;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      VSS.Implementation.UTF8_Strings.Before_First_Character
        (On.Data, Position);
      Self.Lookup_Grapheme_Cluster_Boundaries (Position);
   end Set_Before_First;

   ---------------------
   -- String_Modified --
   ---------------------

   overriding procedure String_Modified
     (Self     : in out Grapheme_Cluster_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      null;
   end String_Modified;

end VSS.Strings.Cursors.Iterators.Grapheme_Clusters;
