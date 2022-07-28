--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Handlers;
with VSS.Strings.Cursors.Markers.Internals;

package body VSS.Strings.Cursors is

   use type VSS.Implementation.Referrers.Magic_String_Access;
   use type VSS.Implementation.Strings.Character_Offset;

   ---------------------
   -- Character_Index --
   ---------------------

   function Character_Index
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Strings.Character_Index'Base is
   begin
      return Self.First_Character_Index;
   end Character_Index;

   ----------------------
   -- Character_Length --
   ----------------------

   overriding function Character_Length
     (Self : Segment_Cursor_Base) return VSS.Strings.Character_Count is
   begin
      if VSS.Implementation.Strings.Is_Invalid (Self.First_Position)
        or else Self.First_Position.Index > Self.Last_Position.Index
      then
         return 0;

      else
         return
           VSS.Strings.Character_Index
             (Self.Last_Position.Index - Self.First_Position.Index + 1);
      end if;
   end Character_Length;

   ----------------------
   -- Character_Length --
   ----------------------

   overriding function Character_Length
     (Self : Segment_Cursor_Limited_Base) return VSS.Strings.Character_Count is
   begin
      if VSS.Implementation.Strings.Is_Invalid (Self.First_Position)
        or else Self.First_Position.Index > Self.Last_Position.Index
      then
         return 0;

      else
         return
           VSS.Strings.Character_Index
             (Self.Last_Position.Index - Self.First_Position.Index + 1);
      end if;
   end Character_Length;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Character_Cursor_Base)
      return VSS.Strings.Character_Index'Base is
   begin
      return VSS.Strings.Character_Index'Base (Self.Position.Index);
   end First_Character_Index;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Character_Cursor_Limited_Base)
      return VSS.Strings.Character_Index'Base is
   begin
      return VSS.Strings.Character_Index'Base (Self.Position.Index);
   end First_Character_Index;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Segment_Cursor_Base) return VSS.Strings.Character_Index'Base is
   begin
      return VSS.Strings.Character_Index'Base (Self.First_Position.Index);
   end First_Character_Index;

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Character_Index'Base is
   begin
      return VSS.Strings.Character_Index'Base (Self.First_Position.Index);
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

   function First_UTF16_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

   begin
      if Position.UTF16_Offset >= 0 then
         return Position.UTF16_Offset;

      else
         return
           VSS.Implementation.Strings.Handler
             (String.Data).First_UTF16_Offset (String.Data, Position);
      end if;
   end First_UTF16_Offset;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        First_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end First_UTF16_Offset;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        First_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end First_UTF16_Offset;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Segment_Cursor_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        First_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.First_Position);
   end First_UTF16_Offset;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        First_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.First_Position);
   end First_UTF16_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   function First_UTF8_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

   begin
      if Position.UTF8_Offset >= 0 then
         return Position.UTF8_Offset;

      else
         return
           VSS.Implementation.Strings.Handler
             (String.Data).First_UTF8_Offset (String.Data, Position);
      end if;
   end First_UTF8_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        First_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end First_UTF8_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        First_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end First_UTF8_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Segment_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        First_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.First_Position);
   end First_UTF8_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        First_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.First_Position);
   end First_UTF8_Offset;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Character_Cursor_Base) is
   begin
      Self.Position := (others => <>);
   end Invalidate;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Self : in out Character_Cursor_Limited_Base) is
   begin
      Self.Position := (others => <>);
   end Invalidate;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Self : in out Segment_Cursor_Base) is
   begin
      Self.First_Position := (others => <>);
      Self.Last_Position  := (others => <>);
   end Invalidate;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Self : in out Segment_Cursor_Limited_Base) is
   begin
      Self.First_Position := (others => <>);
      Self.Last_Position  := (others => <>);
   end Invalidate;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Self : Character_Cursor_Base) return Boolean is
   begin
      return
        Self.Owner /= null
          and not VSS.Implementation.Strings.Is_Invalid (Self.Position);
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Self : Character_Cursor_Limited_Base) return Boolean is
   begin
      return
        Self.Owner /= null
          and not VSS.Implementation.Strings.Is_Invalid (Self.Position);
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid (Self : Segment_Cursor_Base) return Boolean is
   begin
      return
        Self.Owner /= null
          and not VSS.Implementation.Strings.Is_Invalid (Self.First_Position)
          and not VSS.Implementation.Strings.Is_Invalid (Self.Last_Position);
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Self : Segment_Cursor_Limited_Base) return Boolean is
   begin
      return
        Self.Owner /= null
          and not VSS.Implementation.Strings.Is_Invalid (Self.First_Position)
          and not VSS.Implementation.Strings.Is_Invalid (Self.Last_Position);
   end Is_Valid;

   --------------------------
   -- Last_Character_Index --
   --------------------------

   overriding function Last_Character_Index
     (Self : Segment_Cursor_Base) return VSS.Strings.Character_Index'Base is
   begin
      return VSS.Strings.Character_Index'Base (Self.Last_Position.Index);
   end Last_Character_Index;

   --------------------------
   -- Last_Character_Index --
   --------------------------

   overriding function Last_Character_Index
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Strings.Character_Index'Base is
   begin
      return VSS.Strings.Character_Index'Base (Self.Last_Position.Index);
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

   function Last_UTF16_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        VSS.Implementation.Strings.Handler
          (String.Data).Last_UTF16_Offset (String.Data, Position);
   end Last_UTF16_Offset;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        Last_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end Last_UTF16_Offset;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        Last_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end Last_UTF16_Offset;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Segment_Cursor_Base) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        Last_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Last_Position);
   end Last_UTF16_Offset;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        Last_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Last_Position);
   end Last_UTF16_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   function Last_UTF8_Offset
     (String   : not null VSS.Strings.Magic_String_Access;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        VSS.Implementation.Strings.Handler
          (String.Data).Last_UTF8_Offset (String.Data, Position);
   end Last_UTF8_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Character_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        Last_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end Last_UTF8_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Character_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        Last_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Position);
   end Last_UTF8_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Segment_Cursor_Base) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        Last_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Last_Position);
   end Last_UTF8_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Segment_Cursor_Limited_Base)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        Last_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner), Self.Last_Position);
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
