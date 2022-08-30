--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.Implementation.Line_Iterators;
with VSS.Implementation.String_Handlers;
with VSS.Strings.Cursors.Markers.Internals;
with VSS.Strings.Internals;

package body VSS.Strings.Cursors.Iterators.Lines is

   function Terminator_First
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor;
   --  Return cursor of first character of the line terminator sequence.

   function Terminator_Last
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor;
   --  Return cursor of last character of the line terminator sequence.

   procedure Lookup_Line_Boundaries
     (Self          : in out Line_Iterator'Class;
      Position      : VSS.Implementation.Strings.Cursor);
      --  Terminator    : out VSS.Implementation.Strings.Cursor;
      --  Last_Position : out VSS.Implementation.Strings.Cursor);

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

   ------------------------
   -- Element_Terminator --
   ------------------------

   function Element_Terminator
     (Self : Line_Iterator'Class) return VSS.Strings.Virtual_String
   is
      use type VSS.Implementation.Referrers.Magic_String_Access;

      First      : constant VSS.Implementation.Strings.Cursor :=
        Self.Terminator_First;
      Last       : constant VSS.Implementation.Strings.Cursor :=
        Self.Terminator_Last;
      Terminator : VSS.Implementation.Strings.String_Data;

   begin
      if Self.Owner = null
        or VSS.Implementation.Strings.Is_Invalid (First)
        or VSS.Implementation.Strings.Is_Invalid (Last)
      then
         return VSS.Strings.Empty_Virtual_String;

      else
         VSS.Implementation.Strings.Handler
           (VSS.Strings.Magic_String_Access (Self.Owner).Data).Slice
             (VSS.Strings.Magic_String_Access (Self.Owner).Data,
              First,
              Last,
              Terminator);

         return Result : constant VSS.Strings.Virtual_String :=
           VSS.Strings.Internals.To_Virtual_String (Terminator)
         do
            VSS.Implementation.Strings.Unreference (Terminator);
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
      use type VSS.Implementation.Strings.Character_Count;

   begin
      return
        not VSS.Implementation.Strings.Is_Invalid (Self.First_Position)
          and then Self.First_Position.Index
                     <= VSS.Implementation.Strings.Character_Count
                          (VSS.Strings.Magic_String_Access
                             (Self.Owner).Character_Length);
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
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Line_Iterator'Class;
      String          : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False) is
   begin
      Self.Connect (String'Unrestricted_Access);
      Self.Terminators     := Terminators;
      Self.Keep_Terminator := Keep_Terminator;

      Lookup_Line_Boundaries (Self, Position);
   end Initialize;

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
     (Self          : in out Line_Iterator'Class;
      Position      : VSS.Implementation.Strings.Cursor)
   is
      use type VSS.Implementation.Strings.Character_Count;

      Data    : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);
      Current_Position    : VSS.Implementation.Strings.Cursor := Position;
      Dummy   : Boolean;

   begin
      if Current_Position.Index /= 1 then
         --  Going backward till previous line terminator has been found.

         Dummy :=
           Handler.Forward
             (VSS.Strings.Magic_String_Access (Self.Owner).Data,
              Current_Position);
         Lookup_Previous_Line (Self, Current_Position);
         Current_Position := Self.First_Position;
         Dummy :=
           Handler.Backward
             (VSS.Strings.Magic_String_Access (Self.Owner).Data,
              Current_Position);

      else
         --  Rewind to previous character.

         Dummy :=
           Handler.Backward
             (VSS.Strings.Magic_String_Access (Self.Owner).Data,
              Current_Position);
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
      Data    : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);

      Last_Position       : VSS.Implementation.Strings.Cursor;
      Terminator_Position : VSS.Implementation.Strings.Cursor;
      Dummy               : Boolean;

   begin
      if not VSS.Implementation.Line_Iterators.Forward
        (Data,
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
         Dummy := Handler.Backward (Data, Terminator_Position);

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
      Data                : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Last_Position       : VSS.Implementation.Strings.Cursor;
      Terminator_Position : VSS.Implementation.Strings.Cursor;
      Dummy               : Boolean;

   begin
      if not VSS.Implementation.Line_Iterators.Backward
        (Data,
         Self.Terminators,
         Position,
         Self.First_Position,
         Last_Position,
         Terminator_Position)
      then
         raise Program_Error;
      end if;

      if VSS.Implementation.Strings.Is_Invalid (Terminator_Position) then
         Self.Last_Position       := Last_Position;
         Self.Terminator_Position := (others => <>);

      elsif Self.Keep_Terminator then
         Self.Last_Position       := Last_Position;
         Self.Terminator_Position := Terminator_Position;

      else
         Dummy :=
           VSS.Implementation.Strings.Handler
             (Data).Backward (Data, Terminator_Position);

         Self.Last_Position       := Terminator_Position;
         Self.Terminator_Position := Last_Position;
      end if;
   end Lookup_Previous_Line;

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
      Data     : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;
      Position : VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      if Self.Keep_Terminator then
         Position := Self.Terminator_Position;

      else
         Position := Self.Last_Position;
         Success  :=
           VSS.Implementation.Strings.Handler (Data).Forward (Data, Position);
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
      Data     : VSS.Implementation.Strings.String_Data
        renames Owner.Data;
      Position : VSS.Implementation.Strings.Cursor;
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
              VSS.Implementation.Strings.Handler
                (Data).Forward (Data, Position);

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
