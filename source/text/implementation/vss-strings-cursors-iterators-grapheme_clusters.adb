--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Handlers;
with VSS.Implementation.UCD_Core;

with VSS.Strings.Cursors.Markers;
pragma Unreferenced (VSS.Strings.Cursors.Markers);
--  XXX GNAT 20210710: crash without clause above.

package body VSS.Strings.Cursors.Iterators.Grapheme_Clusters is

   use type VSS.Implementation.Strings.Character_Offset;
   use all type VSS.Implementation.UCD_Core.GCB_Values;

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
     (Handler : not null VSS.Implementation.Strings.String_Handler_Access;
      Data    : VSS.Implementation.Strings.String_Data;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Scan string backward to check whether Rules GB12, GB13 should be
   --  applied.

   function Apply_ExtPict
     (Handler : not null VSS.Implementation.Strings.String_Handler_Access;
      Data    : VSS.Implementation.Strings.String_Data;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean;
   --  Can string backward to check whether Rule GB11 should be applied.

   -------------------
   -- Apply_ExtPict --
   -------------------

   function Apply_ExtPict
     (Handler : not null VSS.Implementation.Strings.String_Handler_Access;
      Data    : VSS.Implementation.Strings.String_Data;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position   : VSS.Implementation.Strings.Cursor := Left;
      Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

   begin
      loop
         if not Handler.Backward (Data, Position) then
            return False;
         end if;

         Properties := Extract_Core_Data (Handler.Element (Data, Position));

         if Properties.GCB = GCB_EX then
            null;

         elsif Properties.ExtPict then
            return True;

         else
            return False;
         end if;
      end loop;
   end Apply_ExtPict;

   --------------
   -- Apply_RI --
   --------------

   function Apply_RI
     (Handler : not null VSS.Implementation.Strings.String_Handler_Access;
      Data    : VSS.Implementation.Strings.String_Data;
      Left    : VSS.Implementation.Strings.Cursor) return Boolean
   is
      Position : VSS.Implementation.Strings.Cursor := Left;
      Count    : Natural := 0;

   begin
      loop
         if not Handler.Backward (Data, Position) then
            return Count mod 2 = 0;
         end if;

         if Extract_Core_Data (Handler.Element (Data, Position)).GCB
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

   function Backward
     (Self : in out Grapheme_Cluster_Iterator) return Boolean
   is
      Data             : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Handler          : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);
      Right            : VSS.Implementation.Strings.Cursor;
      Right_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Left             : VSS.Implementation.Strings.Cursor;
      Left_Properties  : VSS.Implementation.UCD_Core.Core_Data_Record;
      Success          : Boolean;
      Done             : Boolean := False;

   begin
      Self.Last_Position := Self.First_Position;
      Success := Handler.Backward (Data, Self.Last_Position);

      if not Success then
         --  Start of the line has been reached.

         Self.First_Position := Self.Last_Position;

         return False;

      else
         Left := Self.Last_Position;
         Left_Properties :=
           Extract_Core_Data (Handler.Element (Data, Left));

         loop
            Right            := Left;
            Right_Properties := Left_Properties;

            Success := Handler.Backward (Data, Left);

            if not Success then
               --  Start of the string has been reached.

               Self.First_Position := Right;

               return True;

            else
               Left_Properties :=
                 Extract_Core_Data (Handler.Element (Data, Left));

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
                 and Right_Properties.GCB in GCB_L | GCB_V | GCB_LV | GCB_LVT
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

               elsif Left_Properties.GCB = GCB_ZWJ
                 and then Right_Properties.ExtPict
                 and then Apply_ExtPict (Handler, Data, Left)
               then
                  --  Rule 11.

                  null;

               elsif Left_Properties.GCB = GCB_RI
                 and then Right_Properties.GCB = GCB_RI
                 and then Apply_RI (Handler, Data, Left)
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

   overriding function Forward
     (Self : in out Grapheme_Cluster_Iterator) return Boolean
   is
      Data             : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Handler          : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);
      Left             : VSS.Implementation.Strings.Cursor;
      Left_Properties  : VSS.Implementation.UCD_Core.Core_Data_Record;
      Right            : VSS.Implementation.Strings.Cursor;
      Right_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Success          : Boolean;
      Done             : Boolean := False;

   begin
      Self.First_Position := Self.Last_Position;
      Success := Handler.Forward (Data, Self.First_Position);

      if not Success then
         --  End of the string has been reached.
         --  XXX Should Last_Position be set to After_Last_Character?

         return False;

      else
         Right    := Self.First_Position;
         Right_Properties :=
           Extract_Core_Data (Handler.Element (Data, Right));

         loop
            Left            := Right;
            Left_Properties := Right_Properties;

            Success := Handler.Forward (Data, Right);

            if not Success then
               --  End of line has been reached
               --  Rule GB2

               Self.Last_Position := Left;

               return True;

            else
               Right_Properties :=
                 Extract_Core_Data (Handler.Element (Data, Right));

               if Left_Properties.GCB = GCB_CR
                 and Right_Properties.GCB = GCB_LF
               then
                  --  Rule GB3

                  null;

               elsif Left_Properties.GCB in GCB_CN | GCB_CR | GCB_LF then
                  --  Rule GB4

                  Done := True;

               elsif Right_Properties.GCB in GCB_CN | GCB_CR | GCB_LF then
                  --  Rule GB5

                  Done := True;

               elsif Left_Properties.GCB = GCB_L
                 and then Right_Properties.GCB
               in GCB_L | GCB_V | GCB_LV | GCB_LVT
               then
                  --  Rule GB6

                  null;

               elsif Left_Properties.GCB in GCB_LV | GCB_V
                 and then Right_Properties.GCB in GCB_V | GCB_T
               then
                  --  Rule GB7

                  null;

               elsif Left_Properties.GCB in GCB_LVT | GCB_T
                 and then Right_Properties.GCB = GCB_T
               then
                  --  Rule GB8

                  null;

               elsif Right_Properties.GCB in GCB_EX | GCB_ZWJ then
                  --  Rule GB9

                  null;

               elsif Right_Properties.GCB = GCB_SM then
                  --  Rule GB9a

                  null;

               elsif Left_Properties.GCB = GCB_PP then
                  --  Rule GB9b

                  null;

               elsif Left_Properties.GCB = GCB_ZWJ
                 and then Right_Properties.ExtPict
                 and then Apply_ExtPict (Handler, Data, Left)
               then
                  --  Rule GB11.

                  null;

               elsif Left_Properties.GCB = GCB_RI
                 and then Right_Properties.GCB = GCB_RI
                 and then Apply_RI (Handler, Data, Left)
               then
                  --  Rule GB12.
                  --  Rule GB13.

                  null;

               else
                  Done := True;
               end if;

               if Done then
                  Self.Last_Position := Left;

                  return True;
               end if;
            end if;
         end loop;
      end if;
   end Forward;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Grapheme_Cluster_Iterator) return Boolean
   is
      Data    : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Handler : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);

   begin
      return
        Self.First_Position.Index in 1 .. Handler.Length (Data);
   end Has_Element;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Grapheme_Cluster_Iterator'Class;
      String          : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor) is
   begin
      Self.Connect (String'Unrestricted_Access);
      Self.Lookup_Grapheme_Cluster_Boundaries (Position);
   end Initialize;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Grapheme_Cluster_Iterator) is
   begin
      Abstract_Segment_Iterator (Self).Invalidate;
   end Invalidate;

   ----------------------------------------
   -- Lookup_Grapheme_Cluster_Boundaries --
   ----------------------------------------

   procedure Lookup_Grapheme_Cluster_Boundaries
     (Self     : in out Grapheme_Cluster_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor)
   is
      Data    : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Handler : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);
      Success : Boolean with Unreferenced;

   begin
      if Position.Index = 0 then
         --  Before first character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index > Handler.Length (Data) then
         --  After last character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index = 1 then
         --  First character of the string, it starts first grapheme cluster.

         Handler.Before_First_Character (Data, Self.First_Position);
         Handler.Before_First_Character (Data, Self.Last_Position);
         Success := Self.Forward;

      elsif Position.Index = Handler.Length (Data) then
         --  Last character of the string, it ends last grapheme cluster.

         Self.Last_Position  := Position;
         Self.First_Position := Position;
         Success := Handler.Forward (Data, Self.First_Position);
         Success := Self.Backward;

      else
         raise Program_Error;
      end if;
   end Lookup_Grapheme_Cluster_Boundaries;

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
