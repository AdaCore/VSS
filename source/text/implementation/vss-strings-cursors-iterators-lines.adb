--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Line_Iterators;
with VSS.Strings.Cursors.Markers.Internals;

package body VSS.Strings.Cursors.Iterators.Lines is

   use type VSS.Implementation.Referrers.Magic_String_Access;

   function Terminator_First
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor;
   --  Return cursor of first character of the line terminator sequence.

   function Terminator_Last
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor;
   --  Return cursor of last character of the line terminator sequence.

   procedure Lookup_Line_Boundaries
     (Self            : in out Line_Iterator'Class;
      Position        : VSS.Implementation.Strings.Cursor;
      Terminators     : Line_Terminator_Set;
      Keep_Terminator : Boolean);

   procedure Lookup_Next_Line
     (Self     : in out Line_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor);
   --  Lookup for next line. Position points to the last character of the
   --  line terminator sequence of the current line.

   procedure Lookup_Previous_Line
     (Self     : in out Line_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor);
   --  Lookup for previous line. Position points to the first character of
   --  the line of the current line.

   --------------
   -- Backward --
   --------------

   overriding function Backward (Self : in out Line_Iterator) return Boolean is
   begin
      Lookup_Previous_Line (Self, Self.First_Position);

      return VSS.Strings.Character_Count (Self.First_Position.Index) /= 0;
   end Backward;

   ------------------------
   -- Element_Terminator --
   ------------------------

   function Element_Terminator
     (Self : Line_Iterator'Class) return VSS.Strings.Virtual_String
   is
      First : constant VSS.Implementation.Strings.Cursor :=
        Self.Terminator_First;
      Last  : constant VSS.Implementation.Strings.Cursor :=
        Self.Terminator_Last;

   begin
      if Self.Owner = null
        or VSS.Implementation.Strings.Is_Invalid (First)
        or VSS.Implementation.Strings.Is_Invalid (Last)
      then
         return VSS.Strings.Empty_Virtual_String;

      else
         return Result : VSS.Strings.Virtual_String do
            VSS.Implementation.UTF8_Strings.Slice
              (VSS.Strings.Magic_String_Access (Self.Owner).Data,
               First,
               Last,
               Result.Data);
         end return;
      end if;
   end Element_Terminator;

   -------------
   -- Forward --
   -------------

   overriding function Forward (Self : in out Line_Iterator) return Boolean is
   begin
      Lookup_Next_Line
        (Self,
         (if Self.Keep_Terminator
            then Self.Last_Position
            elsif VSS.Implementation.Strings.Is_Invalid
                    (Self.Terminator_Position)
              then Self.Last_Position
              else Self.Terminator_Position));

      return
        VSS.Strings.Magic_String_Access (Self.Owner).Character_Length
          >= VSS.Strings.Character_Count (Self.First_Position.Index);
   end Forward;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Self : Line_Iterator) return Boolean is
      Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner = null then
         --  Uninitialized iterator.

         return False;
      end if;

      return Self.First_Position.Index in 1 .. Text.Length;
   end Has_Element;

   -------------------------
   -- Has_Line_Terminator --
   -------------------------

   function Has_Line_Terminator (Self : Line_Iterator'Class) return Boolean is
   begin
      return
        not VSS.Implementation.Strings.Is_Invalid (Self.Terminator_Position);
   end Has_Line_Terminator;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Line_Iterator) is
   begin
      Abstract_Segment_Iterator (Self).Invalidate;

      Self.Terminator_Position := (others => <>);
   end Invalidate;

   ----------------------------
   -- Lookup_Line_Boundaries --
   ----------------------------

   procedure Lookup_Line_Boundaries
     (Self            : in out Line_Iterator'Class;
      Position        : VSS.Implementation.Strings.Cursor;
      Terminators     : Line_Terminator_Set;
      Keep_Terminator : Boolean)
   is
      use type VSS.Implementation.Strings.Character_Count;

      Text             : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Current_Position : aliased VSS.Implementation.Strings.Cursor := Position;
      Dummy            : Boolean;

   begin
      Self.Terminators     := Terminators;
      Self.Keep_Terminator := Keep_Terminator;

      if Position.Index = 0 then
         --  Before first character

         Self.First_Position :=
           VSS.Implementation.Strings.Position_Before_First_Character;
         Self.Last_Position  :=
           VSS.Implementation.Strings.Position_Before_First_Character;

         return;

      elsif Current_Position.Index /= 1 then
         --  Going backward till previous line terminator has been found.

         Dummy :=
           VSS.Implementation.UTF8_Strings.Forward (Text, Current_Position);
         Lookup_Previous_Line (Self, Current_Position);
         Current_Position := Self.First_Position;
         Dummy :=
           VSS.Implementation.UTF8_Strings.Backward (Text, Current_Position);

      else
         --  Rewind to previous character.

         Dummy :=
           VSS.Implementation.UTF8_Strings.Backward (Text, Current_Position);
      end if;

      Lookup_Next_Line (Self, Current_Position);
   end Lookup_Line_Boundaries;

   ----------------------
   -- Lookup_Next_Line --
   ----------------------

   procedure Lookup_Next_Line
     (Self     : in out Line_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor)
   is
      Text                : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Last_Position       : aliased VSS.Implementation.Strings.Cursor;
      Terminator_Position : aliased VSS.Implementation.Strings.Cursor;
      Dummy               : Boolean;

   begin
      if not VSS.Implementation.Line_Iterators.Forward
        (Text,
         Self.Terminators,
         Position,
         Self.First_Position,
         Last_Position,
         Terminator_Position)
      then
         Self.Last_Position       := Position;
         Self.Terminator_Position := (others => <>);

         return;
      end if;

      if VSS.Implementation.Strings.Is_Invalid (Terminator_Position) then
         --  Line terminator sequence is not found, and end of string
         --  reached.

         Self.Last_Position       := Last_Position;
         Self.Terminator_Position := (others => <>);

      elsif Self.Keep_Terminator then
         Self.Last_Position       := Last_Position;
         Self.Terminator_Position := Terminator_Position;

      else
         Dummy :=
           VSS.Implementation.UTF8_Strings.Backward
             (Text, Terminator_Position);

         Self.Last_Position       := Terminator_Position;
         Self.Terminator_Position := Last_Position;
      end if;
   end Lookup_Next_Line;

   --------------------------
   -- Lookup_Previous_Line --
   --------------------------

   procedure Lookup_Previous_Line
     (Self     : in out Line_Iterator'Class;
      Position : VSS.Implementation.Strings.Cursor)
   is
      Text                : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Last_Position       : VSS.Implementation.Strings.Cursor;
      Terminator_Position : aliased VSS.Implementation.Strings.Cursor;
      Dummy               : Boolean;

   begin
      Dummy :=
        VSS.Implementation.Line_Iterators.Backward
          (Text,
           Self.Terminators,
           Position,
           Self.First_Position,
           Last_Position,
           Terminator_Position);

      if VSS.Implementation.Strings.Is_Invalid (Terminator_Position) then
         Self.Last_Position       := Last_Position;
         Self.Terminator_Position := (others => <>);

      elsif Self.Keep_Terminator then
         Self.Last_Position       := Last_Position;
         Self.Terminator_Position := Terminator_Position;

      else
         Dummy :=
           VSS.Implementation.UTF8_Strings.Backward
             (Text, Terminator_Position);

         Self.Last_Position       := Terminator_Position;
         Self.Terminator_Position := Last_Position;
      end if;
   end Lookup_Previous_Line;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (Self            : in out Line_Iterator;
      Position        : VSS.Strings.Cursors.Abstract_Character_Cursor'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False)
   is
      Cursor_Owner    : VSS.Implementation.Referrers.Magic_String_Access;
      Cursor_Position : VSS.Implementation.Strings.Cursor;

   begin
      Get_Owner_And_Position (Position, Cursor_Owner, Cursor_Position);

      Self.Reconnect (Cursor_Owner);

      if Self.Owner /= null then
         Self.Lookup_Line_Boundaries
           (Cursor_Position, Terminators, Keep_Terminator);

      else
         Self.Invalidate;
      end if;
   end Set_At;

   ------------------
   -- Set_At_First --
   ------------------

   procedure Set_At_First
     (Self            : in out Line_Iterator;
      On              : VSS.Strings.Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      VSS.Implementation.UTF8_Strings.Before_First_Character
        (On.Data, Position);
      Dummy := VSS.Implementation.UTF8_Strings.Forward (On.Data, Position);
      Self.Lookup_Line_Boundaries (Position, Terminators, Keep_Terminator);
   end Set_At_First;

   -----------------
   -- Set_At_Last --
   -----------------

   procedure Set_At_Last
     (Self            : in out Line_Iterator;
      On              : VSS.Strings.Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);

      VSS.Implementation.UTF8_Strings.After_Last_Character (On.Data, Position);
      Dummy := VSS.Implementation.UTF8_Strings.Backward (On.Data, Position);
      Self.Lookup_Line_Boundaries (Position, Terminators, Keep_Terminator);
   end Set_At_Last;

   ---------------------
   -- String_Modified --
   ---------------------

   overriding procedure String_Modified
     (Self     : in out Line_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      null;
   end String_Modified;

   ----------------------
   -- Terminator_First --
   ----------------------

   function Terminator_First
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor
   is
      Text     : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Position : aliased VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      if Self.Keep_Terminator then
         Position := Self.Terminator_Position;

      else
         Position := Self.Last_Position;
         Success  := VSS.Implementation.UTF8_Strings.Forward (Text, Position);
      end if;

      return Position;
   end Terminator_First;

   --------------------------------------
   -- Terminator_First_Character_Index --
   --------------------------------------

   function Terminator_First_Character_Index
     (Self : Line_Iterator'Class)
      return VSS.Strings.Character_Index
   is
      Owner    : VSS.Strings.Magic_String_Access
        renames VSS.Strings.Magic_String_Access (Self.Owner);
      Text     : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames Owner.Data;
      Position : aliased VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      if VSS.Implementation.Strings.Is_Invalid (Self.Terminator_Position) then
         return Owner.Character_Length + 1;

      else
         if Self.Keep_Terminator then
            return
              VSS.Strings.Character_Index (Self.Terminator_Position.Index);

         else
            Position := Self.Last_Position;
            Success :=
              VSS.Implementation.UTF8_Strings.Forward (Text, Position);

            return VSS.Strings.Character_Count (Position.Index);
         end if;
      end if;
   end Terminator_First_Character_Index;

   -----------------------------
   -- Terminator_First_Marker --
   -----------------------------

   function Terminator_First_Marker
     (Self : Line_Iterator'Class)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.Terminator_First);
   end Terminator_First_Marker;

   -----------------------------------
   -- Terminator_First_UTF16_Offset --
   -----------------------------------

   function Terminator_First_UTF16_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        First_UTF16_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner),
           Self.Terminator_First);
   end Terminator_First_UTF16_Offset;

   ----------------------------------
   -- Terminator_First_UTF8_Offset --
   ----------------------------------

   function Terminator_First_UTF8_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        First_UTF8_Offset
          (VSS.Strings.Magic_String_Access (Self.Owner),
           Self.Terminator_First);
   end Terminator_First_UTF8_Offset;

   ---------------------
   -- Terminator_Last --
   ---------------------

   function Terminator_Last
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor is
   begin
      return
        (if Self.Keep_Terminator
           then Self.Last_Position
           else Self.Terminator_Position);
   end Terminator_Last;

   -------------------------------------
   -- Terminator_Last_Character_Index --
   -------------------------------------

   function Terminator_Last_Character_Index
     (Self : Line_Iterator'Class)
      return VSS.Strings.Character_Index is
   begin
      if VSS.Implementation.Strings.Is_Invalid (Self.Terminator_Position) then
         return VSS.Strings.Magic_String_Access (Self.Owner).Character_Length;

      else
         if Self.Keep_Terminator then
            return VSS.Strings.Character_Index (Self.Last_Position.Index);

         else
            return
              VSS.Strings.Character_Index (Self.Terminator_Position.Index);
         end if;
      end if;
   end Terminator_Last_Character_Index;

   ----------------------------
   -- Terminator_Last_Marker --
   ----------------------------

   function Terminator_Last_Marker
     (Self : Line_Iterator'Class)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Character_Marker
          (Self.Owner.all, Self.Terminator_Last);
   end Terminator_Last_Marker;

   ----------------------------------
   -- Terminator_Last_UTF16_Offset --
   ----------------------------------

   function Terminator_Last_UTF16_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      raise Program_Error;
      --  XXX Not implemented.
      return 0;
   end Terminator_Last_UTF16_Offset;

   ---------------------------------
   -- Terminator_Last_UTF8_Offset --
   ---------------------------------

   function Terminator_Last_UTF8_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      raise Program_Error;
      --  XXX Not implemented.
      return 0;
   end Terminator_Last_UTF8_Offset;

end VSS.Strings.Cursors.Iterators.Lines;
