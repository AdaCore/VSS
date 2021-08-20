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

   --------------
   -- Backward --
   --------------

   function Backward
     (Self : in out Grapheme_Cluster_Iterator) return Boolean
   is
      Handler             : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Owner.Data);
      First               : VSS.Implementation.Strings.Cursor;
      First_Properties    : VSS.Implementation.UCD_Core.Core_Data_Record;
      Previous            : VSS.Implementation.Strings.Cursor;
      Previous_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Success             : Boolean;
      Done                : Boolean := False;

      function Apply_RI return Boolean;
      --  Check whether Rules GB12, GB13 should be applied.

      function Apply_ExtPict return Boolean;
      --  Check whether Rule GB11 should be applied.

      -------------------
      -- Apply_ExtPict --
      -------------------

      function Apply_ExtPict return Boolean is
         Position   : VSS.Implementation.Strings.Cursor := Previous;
         Properties : VSS.Implementation.UCD_Core.Core_Data_Record;

      begin
         loop
            if not Handler.Backward (Self.Owner.Data, Position) then
               return False;
            end if;

            Properties :=
              Extract_Core_Data
                (Handler.Element (Self.Owner.Data, Position));

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

      function Apply_RI return Boolean is
         Position : VSS.Implementation.Strings.Cursor := Previous;
         Count    : Natural := 0;

      begin
         loop
            if not Handler.Backward (Self.Owner.Data, Position) then
               return Count mod 2 = 0;
            end if;

            if Extract_Core_Data
                 (Handler.Element (Self.Owner.Data, Position)).GCB = GCB_RI
            then
               Count := Count + 1;

            else
               return Count mod 2 = 0;
            end if;
         end loop;
      end Apply_RI;

   begin
      Self.Last_Position := Self.First_Position;
      Success := Handler.Backward (Self.Owner.Data, Self.Last_Position);

      if not Success then
         --  Start of the line has been reached.

         Self.First_Position := Self.Last_Position;

         return False;

      else
         Previous := Self.Last_Position;
         Previous_Properties :=
           Extract_Core_Data (Handler.Element (Self.Owner.Data, Previous));

         loop
            First            := Previous;
            First_Properties := Previous_Properties;

            Success := Handler.Backward (Self.Owner.Data, Previous);

            if not Success then
               --  Start of the string has been reached.

               Self.First_Position := First;

               return True;

            else
               Previous_Properties :=
                 Extract_Core_Data
                   (Handler.Element (Self.Owner.Data, Previous));

               if Previous_Properties.GCB = GCB_CR
                 and First_Properties.GCB = GCB_LF
               then
                  --  Rule GB3.

                  null;

               elsif Previous_Properties.GCB in GCB_CN | GCB_CR | GCB_LF then
                  --  Rule GB4.

                  Done := True;

               elsif First_Properties.GCB in  GCB_CN | GCB_CR | GCB_LF then
                  --  Rule GB5.

                  Done := True;

               elsif Previous_Properties.GCB = GCB_L
                 and First_Properties.GCB in GCB_L | GCB_V | GCB_LV | GCB_LVT
               then
                  --  Rule GB6.

                  null;

               elsif Previous_Properties.GCB in GCB_LV | GCB_V
                 and First_Properties.GCB in GCB_V | GCB_T
               then
                  --  Rule GB7.

                  null;

               elsif Previous_Properties.GCB in GCB_LVT | GCB_T
                 and First_Properties.GCB in GCB_T
               then
                  --  Rule GB8.

                  null;

               elsif First_Properties.GCB in GCB_EX | GCB_ZWJ then
                  --  Rule GB9.

                  null;

               elsif First_Properties.GCB = GCB_SM then
                  --  Rule 9a.

                  null;

               elsif Previous_Properties.GCB = GCB_PP then
                  --  Rule 9b.

                  null;

               elsif Previous_Properties.GCB = GCB_ZWJ
                 and then First_Properties.ExtPict
                 and then Apply_ExtPict
               then
                  --  Rule 11.

                  null;

               elsif Previous_Properties.GCB = GCB_RI
                 and then First_Properties.GCB = GCB_RI
                 and then Apply_RI
               then
                  --  Rules GB12, GB13.

                  null;

               else
                  --  Rule GB999.

                  Done := True;
               end if;

               if Done then
                  Self.First_Position := First;

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
      type ExtPict_State is
        (None,
         Started,
         Matched,
         Apply);

      type RI_State is
        (None,
         Apply);

      Handler         : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Owner.Data);
      Last            : VSS.Implementation.Strings.Cursor;
      Last_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Next            : VSS.Implementation.Strings.Cursor;
      Next_Properties : VSS.Implementation.UCD_Core.Core_Data_Record;
      Success         : Boolean;
      Done            : Boolean := False;
      RI              : RI_State := None;
      ExtPict         : ExtPict_State := None;

   begin
      Self.First_Position := Self.Last_Position;
      Success := Handler.Forward (Self.Owner.Data, Self.First_Position);

      if not Success then
         --  End of the string has been reached.
         --  XXX Should Last_Position be set to After_Last_Character?

         return False;

      else
         Next    := Self.First_Position;
         Next_Properties :=
           Extract_Core_Data (Handler.Element (Self.Owner.Data, Next));

         loop
            Last            := Next;
            Last_Properties := Next_Properties;

            Success := Handler.Forward (Self.Owner.Data, Next);

            if not Success then
               --  End of line has been reached
               --  Rule GB2

               Self.Last_Position := Last;

               return True;

            else
               Next_Properties :=
                 Extract_Core_Data (Handler.Element (Self.Owner.Data, Next));

               --  Process context for Rule GB11

               if Last_Properties.ExtPict
                 and then Next_Properties.GCB in GCB_EX
               then
                  --  Before context for Rule GB11 has been started.

                  ExtPict := Started;

               elsif Last_Properties.ExtPict
                 and then Next_Properties.GCB in GCB_ZWJ
               then
                  --  Before context for Rule GB11 has been found.

                  ExtPict := Matched;

               elsif ExtPict = Started then
                  if Next_Properties.GCB = GCB_EX then
                     --  Consume GCB_EX character, it doesn't change state.

                     null;

                  elsif Next_Properties.GCB = GCB_ZWJ then
                     --  GCB_ZWJ complete match of the context

                     ExtPict := Matched;

                  else
                     --  Any other character reset context

                     ExtPict := None;
                  end if;

               elsif ExtPict = Matched then
                  ExtPict := Apply;
               end if;

               --  Process context for Rules GB12, GB13.

               if Last_Properties.GCB = GCB_RI
                 and Next_Properties.GCB = GCB_RI
                 and RI = None
               then
                  RI := Apply;

               elsif RI /= None then
                  RI := None;
               end if;

               if Last_Properties.GCB = GCB_CR
                 and Next_Properties.GCB = GCB_LF
               then
                  --  Rule GB3

                  null;

               elsif Last_Properties.GCB in GCB_CN | GCB_CR | GCB_LF then
                  --  Rule GB4

                  Done := True;

               elsif Next_Properties.GCB in GCB_CN | GCB_CR | GCB_LF then
                  --  Rule GB5

                  Done := True;

               elsif Last_Properties.GCB = GCB_L
                 and then Next_Properties.GCB
               in GCB_L | GCB_V | GCB_LV | GCB_LVT
               then
                  --  Rule GB6

                  null;

               elsif Last_Properties.GCB in GCB_LV | GCB_V
                 and then Next_Properties.GCB in GCB_V | GCB_T
               then
                  --  Rule GB7

                  null;

               elsif Last_Properties.GCB in GCB_LVT | GCB_T
                 and then Next_Properties.GCB = GCB_T
               then
                  --  Rule GB8

                  null;

               elsif Next_Properties.GCB in GCB_EX | GCB_ZWJ then
                  --  Rule GB9

                  null;

               elsif Next_Properties.GCB = GCB_SM then
                  --  Rule GB9a

                  null;

               elsif Last_Properties.GCB = GCB_PP then
                  --  Rule GB9b

                  null;

               elsif ExtPict = Apply and Next_Properties.ExtPict then
                  --  Rule GB11.

                  null;

               elsif RI = Apply then
                  --  Rule GB12.
                  --  Rule GB13.

                  null;

               else
                  Done := True;
               end if;

               if Done then
                  Self.Last_Position := Last;

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
      Handler : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Owner.Data);

   begin
      return
        Self.First_Position.Index in 1 .. Handler.Length (Self.Owner.Data);
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
      Handler : constant not null
        VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Owner.Data);
      Success : Boolean with Unreferenced;

   begin
      if Position.Index = 0 then
         --  Before first character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index > Handler.Length (Self.Owner.Data) then
         --  After last character of the string.

         Self.First_Position := Position;
         Self.Last_Position  := Position;

      elsif Position.Index = 1 then
         --  First character of the string, it starts first grapheme cluster.

         Handler.Before_First_Character (Self.Owner.Data, Self.First_Position);
         Handler.Before_First_Character (Self.Owner.Data, Self.Last_Position);
         Success := Self.Forward;

      elsif Position.Index = Handler.Length (Self.Owner.Data) then
         --  Last character of the string, it ends last grapheme cluster.

         Self.Last_Position  := Position;
         Self.First_Position := Position;
         Success := Handler.Forward (Self.Owner.Data, Self.First_Position);
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
