------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with VSS.Strings.Cursors.Markers.Internals;

package body VSS.Strings.Cursors is

   ---------------------
   -- Character_Index --
   ---------------------

   function Character_Index
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Strings.Character_Index is
   begin
      return Self.First_Character_Index;
   end Character_Index;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Character_Cursor_Base)
      return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Index (Self.Position.Index);
   end First_Character_Index;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Character_Cursor_Limited_Base)
      return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Index (Self.Position.Index);
   end First_Character_Index;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Segment_Cursor_Base) return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Index (Self.First_Position.Index);
   end First_Character_Index;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Segment_Cursor_Limited_Base) return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Index (Self.First_Position.Index);
   end First_Character_Index;

   ------------------
   -- First_Marker --
   ------------------

   overriding function First_Marker
     (Self : Character_Cursor_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.Position);
   end First_Marker;

   ------------------
   -- First_Marker --
   ------------------

   overriding function First_Marker
     (Self : Character_Cursor_Limited_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.Position);
   end First_Marker;

   ------------------
   -- First_Marker --
   ------------------

   overriding function First_Marker
     (Self : Segment_Cursor_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.First_Position);
   end First_Marker;

   ------------------
   -- First_Marker --
   ------------------

   overriding function First_Marker
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.First_Position);
   end First_Marker;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return Self.Position.UTF16_Offset;
   end First_UTF16_Offset;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return Self.Position.UTF16_Offset;
   end First_UTF16_Offset;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Segment_Cursor_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return Self.First_Position.UTF16_Offset;
   end First_UTF16_Offset;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return Self.First_Position.UTF16_Offset;
   end First_UTF16_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return Self.Position.UTF8_Offset;
   end First_UTF8_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return Self.Position.UTF8_Offset;
   end First_UTF8_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Segment_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return Self.First_Position.UTF8_Offset;
   end First_UTF8_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return Self.First_Position.UTF8_Offset;
   end First_UTF8_Offset;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Character_Cursor_Base) is
   begin
      Self.Position := (1, 0, 0);
   end Invalidate;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Self : in out Character_Cursor_Limited_Base) is
   begin
      Self.Position := (1, 0, 0);
   end Invalidate;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Self : in out Segment_Cursor_Base) is
   begin
      Self.First_Position := (1, 0, 0);
      Self.Last_Position  := (1, 0, 0);
   end Invalidate;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Self : in out Segment_Cursor_Limited_Base) is
   begin
      Self.First_Position := (1, 0, 0);
      Self.Last_Position  := (1, 0, 0);
   end Invalidate;

   --------------------------
   -- Last_Character_Index --
   --------------------------

   overriding function Last_Character_Index
     (Self : Segment_Cursor_Base) return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Index (Self.Last_Position.Index);
   end Last_Character_Index;

   --------------------------
   -- Last_Character_Index --
   --------------------------

   overriding function Last_Character_Index
     (Self : Segment_Cursor_Limited_Base) return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Index (Self.Last_Position.Index);
   end Last_Character_Index;

   -----------------
   -- Last_Marker --
   -----------------

   overriding function Last_Marker
     (Self : Segment_Cursor_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.Last_Position);
   end Last_Marker;

   -----------------
   -- Last_Marker --
   -----------------

   overriding function Last_Marker
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.Last_Position);
   end Last_Marker;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Position.UTF16_Offset;
   end Last_UTF16_Offset;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Position.UTF16_Offset;
   end Last_UTF16_Offset;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Segment_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Last_Position.UTF16_Offset;
   end Last_UTF16_Offset;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Last_Position.UTF16_Offset;
   end Last_UTF16_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Position.UTF8_Offset;
   end Last_UTF8_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Position.UTF8_Offset;
   end Last_UTF8_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Segment_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Last_Position.UTF8_Offset;
   end Last_UTF8_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Last_Position.UTF8_Offset;
   end Last_UTF8_Offset;

   ------------
   -- Marker --
   ------------

   function Marker
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return Self.First_Marker;
   end Marker;

end VSS.Strings.Cursors;
