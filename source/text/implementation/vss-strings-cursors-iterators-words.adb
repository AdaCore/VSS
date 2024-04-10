--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Text_Handlers;
with VSS.Implementation.UCD_Core;

with VSS.Strings.Cursors.Markers;
pragma Unreferenced (VSS.Strings.Cursors.Markers);
--  XXX GNAT 20230326: crash without clause above.

package body VSS.Strings.Cursors.Iterators.Words is

   use type VSS.Implementation.Referrers.Magic_String_Access;
   use type VSS.Implementation.Strings.Character_Offset;
   use all type VSS.Implementation.UCD_Core.WB_Values;

   procedure Lookup_Word_Boundaries
     (Self     : in out Word_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor);
   --  Lookup for word boundaries around given position and setup
   --  iterator to point to found segment.

   function Extract_Core_Data
     (Code : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Core.Core_Data_Record;
   --  Return core data record for the given character.

   function Apply_WB6
     (Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Right   : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string forward to check whether Rule WB6 should be applied.

   function Apply_WB7
     (Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string backward to check whether Rule WB7 should be applied.

   function Apply_WB7b
     (Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Right   : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string forward to check whether Rule WB7b should be applied.

   function Apply_WB7c
     (Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string backward to check whether Rule WB7c should be applied.

   function Apply_WB11
     (Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string backward to check whether Rule WB7 should be applied.

   function Apply_WB12
     (Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Right   : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string forward to check whether Rule WB12 should be applied.

   function Apply_WB15_WB16
     (Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string backward to check whether Rules WB15, WB16 should be
   --  applied.

   ----------------
   -- Apply_WB11 --
   ----------------

   function Apply_WB11
     (Handler : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : VSS.Implementation.Strings.Cursor := Left;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not Handler.Backward (Position) then
            return False;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Position));

         exit when Properties.WB not in WB_Extend | WB_FO | WB_ZWJ;
      end loop;

      return Properties.WB = WB_NU;
   end Apply_WB11;

   ----------------
   -- Apply_WB12 --
   ----------------

   function Apply_WB12
     (Handler : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Right   : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : aliased VSS.Implementation.Strings.Cursor := Right;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not Handler.Forward (Position) then
            return False;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Position));

         exit when Properties.WB not in WB_Extend | WB_FO | WB_ZWJ;
      end loop;

      return Properties.WB = WB_NU;
   end Apply_WB12;

   ---------------------
   -- Apply_WB15_WB16 --
   ---------------------

   function Apply_WB15_WB16
     (Handler : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : VSS.Implementation.Strings.Cursor := Left;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Count      : Natural := 0;

   begin
      loop
         if not Handler.Backward (Position) then
            return Count mod 2 = 0;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Position));

         if Properties.WB = WB_RI then
            Count := Count + 1;

         elsif Properties.WB in WB_Extend | WB_FO | WB_ZWJ then
            null;

         else
            return Count mod 2 = 0;
         end if;
      end loop;
   end Apply_WB15_WB16;

   ---------------
   -- Apply_WB6 --
   ---------------

   function Apply_WB6
     (Handler : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Right   : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : aliased VSS.Implementation.Strings.Cursor := Right;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not Handler.Forward (Position) then
            return False;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Position));

         exit when Properties.WB not in WB_Extend | WB_FO | WB_ZWJ;
      end loop;

      return Properties.WB in WB_LE | WB_HL;
   end Apply_WB6;

   ---------------
   -- Apply_WB7 --
   ---------------

   function Apply_WB7
     (Handler : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : VSS.Implementation.Strings.Cursor := Left;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not Handler.Backward (Position) then
            return False;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Position));

         exit when Properties.WB not in WB_Extend | WB_FO | WB_ZWJ;
      end loop;

      return Properties.WB in WB_LE | WB_HL;
   end Apply_WB7;

   ----------------
   -- Apply_WB7b --
   ----------------

   function Apply_WB7b
     (Handler : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Right   : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : aliased VSS.Implementation.Strings.Cursor := Right;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not Handler.Forward (Position) then
            return False;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Position));

         exit when Properties.WB not in WB_Extend | WB_FO | WB_ZWJ;
      end loop;

      return Properties.WB = WB_HL;
   end Apply_WB7b;

   ----------------
   -- Apply_WB7c --
   ----------------

   function Apply_WB7c
     (Handler : VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : VSS.Implementation.Strings.Cursor := Left;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not Handler.Backward (Position) then
            return False;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Position));

         exit when Properties.WB not in WB_Extend | WB_FO | WB_ZWJ;
      end loop;

      return Properties.WB in WB_HL;
   end Apply_WB7c;

   --------------
   -- Backward --
   --------------

   function Backward (Self : in out Word_Iterator) return Boolean is
   begin
      raise Program_Error;
      return Self.Backward;
   end Backward;

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

   overriding function Forward (Self : in out Word_Iterator) return Boolean is
      Data               : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Handler            : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Data);
      Left               : VSS.Implementation.Strings.Cursor;
      Left_Properties    : VSS.Implementation.UCD_Core.Core_Data_Record;
      Right              : aliased VSS.Implementation.Strings.Cursor;
      Right_Properties   : VSS.Implementation.UCD_Core.Core_Data_Record;
      Starter            : VSS.Implementation.Strings.Cursor;
      Starter_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Success            : Boolean;
      Done               : Boolean := False;

   begin
      Self.Kind := Text;
      Self.First_Position := Self.Last_Position;
      Success := Handler.Forward (Self.First_Position);

      if not Success then
         --  End of the string has been reached.
         --  XXX Should Last_Position be set to After_Last_Character?

         return False;
      end if;

      Right            := Self.First_Position;
      Right_Properties := Extract_Core_Data (Handler.Element (Right));

      Self.Kind :=
        (case Right_Properties.WB is
            when WB_WSegSpace          => Whitespace,
            when WB_CR | WB_LF | WB_NL => Line_Break,
            when others                => Text);

      loop
         Left            := Right;
         Left_Properties := Right_Properties;

         Success := Handler.Forward (Right);

         if not Success then
            --  End of line has been reached
            --  Rule WB2

            Self.Last_Position := Left;

            return True;
         end if;

         Right_Properties := Extract_Core_Data (Handler.Element (Right));

         if Left_Properties.WB = WB_CR
           and Right_Properties.WB = WB_LF
         then
            --  Rule WB3

            goto Consumed;

         elsif Left_Properties.WB in WB_NL | WB_CR | WB_LF then
            --  Rule WB3a

            Done := True;

            goto Consumed;

         elsif Right_Properties.WB in WB_NL | WB_CR | WB_LF then
            --  Rule WB3b

            Done := True;

            goto Consumed;

         elsif Left_Properties.WB = WB_ZWJ
           and Right_Properties.ExtPict
         then
            --  Rule WB3c

            goto Consumed;

         elsif Left_Properties.WB = WB_WSegSpace
           and Right_Properties.WB = WB_WSegSpace
         then
            --  Rule WB3d

            goto Consumed;
         end if;

         Starter            := Left;
         Starter_Properties := Left_Properties;

         if Right_Properties.WB in WB_Extend | WB_FO | WB_ZWJ then
            --  Rule WB4

            Self.Kind := Text;
            --  Whitespace might be followed by combinig character(s), such
            --  text segment is not reported as whitespace.

            loop
               Left            := Right;
               Left_Properties := Right_Properties;

               Success := Handler.Forward (Right);

               if not Success then
                  --  End of the string is reached
                  --  Rule WB2

                  Done := True;

                  goto Consumed;
               end if;

               Right_Properties := Extract_Core_Data (Handler.Element (Right));

               exit when Right_Properties.WB not in WB_Extend | WB_FO | WB_ZWJ;
            end loop;
         end if;

         if Left_Properties.WB = WB_ZWJ
           and Right_Properties.ExtPict
         then
            --  Rule WB3c
            --
            --  It is not clear why this rules should be applied here, after
            --
            --     X (Extend | Format | ZWJ)* -> X
            --
            --  conversion, but it is required for one of WordBreakTest.txt
            --  testcase.

            null;

         elsif Starter_Properties.WB in WB_LE | WB_HL
           and Right_Properties.WB in WB_LE | WB_HL
         then
            --  Rule WB5

            null;

         elsif (Starter_Properties.WB in WB_LE | WB_HL
                and Right_Properties.WB in WB_ML | WB_MB | WB_SQ)
           and then Apply_WB6 (Handler.all, Right)
         then
            --  Rule WB6

            null;

         elsif (Starter_Properties.WB in WB_ML | WB_MB | WB_SQ
                and Right_Properties.WB in WB_LE | WB_HL)
           and then Apply_WB7 (Handler.all, Starter)
         then
            --  Rule WB7

            null;

         elsif Starter_Properties.WB = WB_HL
           and Right_Properties.WB = WB_SQ
         then
            --  Rule WB7a

            null;

         elsif (Starter_Properties.WB = WB_HL
                and Right_Properties.WB = WB_DQ)
           and then Apply_WB7b (Handler.all, Right)
         then
            --  Rule WB7b

            null;

         elsif (Starter_Properties.WB = WB_DQ
                and Right_Properties.WB = WB_HL)
           and then Apply_WB7c (Handler.all, Starter)
         then
            --  Rule WB7c

            null;

         elsif Starter_Properties.WB = WB_NU
           and Right_Properties.WB = WB_NU
         then
            --  Rule WB8

            null;

         elsif Starter_Properties.WB in WB_LE | WB_HL
           and Right_Properties.WB = WB_NU
         then
            --  Rule WB9

            null;

         elsif Starter_Properties.WB = WB_NU
           and Right_Properties.WB in WB_LE | WB_HL
         then
            --  Rule WB10

            null;

         elsif (Starter_Properties.WB in WB_MN | WB_MB | WB_SQ
                and Right_Properties.WB = WB_NU)
           and then Apply_WB11 (Handler.all, Starter)
         then
            --  Rule WB11

            null;

         elsif (Starter_Properties.WB = WB_NU
                and Right_Properties.WB in WB_MN | WB_MB | WB_SQ)
           and then Apply_WB12 (Handler.all, Right)
         then
            --  Rule WB12

            null;

         elsif Starter_Properties.WB = WB_KA
           and Right_Properties.WB = WB_KA
         then
            --  Rule WB13

            null;

         elsif Starter_Properties.WB in WB_LE | WB_HL | WB_NU | WB_KA | WB_EX
           and Right_Properties.WB = WB_EX
         then
            --  Rule WB13a

            null;

         elsif Starter_Properties.WB = WB_EX
           and Right_Properties.WB in WB_LE | WB_HL | WB_NU | WB_KA
         then
            --  Rule WB13b

            null;

         elsif (Starter_Properties.WB = WB_RI
                and Right_Properties.WB = WB_RI)
           and then Apply_WB15_WB16 (Handler.all, Starter)
         then
            --  Rule WB15
            --  Rule WB16

            null;

         else
            Done := True;
         end if;

         <<Consumed>>

         if Done then
            Self.Last_Position := Left;

            return True;
         end if;
      end loop;
   end Forward;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Self : Word_Iterator) return Boolean is
      Data : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Text : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Data);

   begin
      return Self.First_Position.Index in 1 .. Text.Length;
   end Has_Element;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Word_Iterator) is
   begin
      Abstract_Segment_Iterator (Self).Invalidate;
   end Invalidate;

   ----------------------------
   -- Lookup_Word_Boundaries --
   ----------------------------

   procedure Lookup_Word_Boundaries
     (Self     : in out Word_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor)
   is
      Data    : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Text    : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Data);
      Success : Boolean with Unreferenced;

   begin
      if Position.Index = 0 then
         --  Before the first character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index > Text.Length then
         --  After last character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index = 1 then
         --  First character of the string, it starts first grapheme cluster.

         Text.Before_First_Character (Self.First_Position);
         Text.Before_First_Character (Self.Last_Position);
         Success := Self.Forward;

      elsif Position.Index = Text.Length then
         raise Program_Error;

      else
         raise Program_Error;
      end if;
   end Lookup_Word_Boundaries;

   -------------------
   -- On_Line_Break --
   -------------------

   function On_Line_Break (Self : Word_Iterator'Class) return Boolean is
   begin
      return Self.Has_Element and then Self.Kind = Line_Break;
   end On_Line_Break;

   -------------------
   -- On_Whitespace --
   -------------------

   function On_Whitespace (Self : Word_Iterator'Class) return Boolean is
   begin
      return Self.Has_Element and then Self.Kind = Whitespace;
   end On_Whitespace;

   --------------------
   -- Set_After_Last --
   --------------------

   procedure Set_After_Last
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler  : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (On.Data);
      Position : VSS.Implementation.Strings.Cursor;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      Handler.After_Last_Character (On.Data, Position);
      Self.Lookup_Word_Boundaries (Position);
   end Set_After_Last;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (Self     : in out Word_Iterator'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
   is
      Cursor_Owner    : VSS.Implementation.Referrers.Magic_String_Access;
      Cursor_Position : VSS.Implementation.Strings.Cursor;

   begin
      Get_Owner_And_Position (Position, Cursor_Owner, Cursor_Position);

      Self.Reconnect (Cursor_Owner);

      if Self.Owner /= null then
         Self.Lookup_Word_Boundaries (Cursor_Position);

      else
         Self.Invalidate;
      end if;
   end Set_At;

   ------------------
   -- Set_At_First --
   ------------------

   procedure Set_At_First
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler  : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (On.Data);
      Position : aliased VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      Handler.Before_First_Character (Position);
      Dummy := Handler.Forward (Position);
      Self.Lookup_Word_Boundaries (Position);
   end Set_At_First;

   -----------------
   -- Set_At_Last --
   -----------------

   procedure Set_At_Last
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler  : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (On.Data);
      Position : VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      Handler.After_Last_Character (On.Data, Position);
      Dummy := Handler.Backward (Position);
      Self.Lookup_Word_Boundaries (Position);
   end Set_At_Last;

   ----------------------
   -- Set_Before_First --
   ----------------------

   procedure Set_Before_First
     (Self : in out Word_Iterator'Class;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler  : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (On.Data);
      Position : VSS.Implementation.Strings.Cursor;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      Handler.Before_First_Character (Position);
      Self.Lookup_Word_Boundaries (Position);
   end Set_Before_First;

   ---------------------
   -- String_Modified --
   ---------------------

   overriding procedure String_Modified
     (Self     : in out Word_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      null;
   end String_Modified;

end VSS.Strings.Cursors.Iterators.Words;
