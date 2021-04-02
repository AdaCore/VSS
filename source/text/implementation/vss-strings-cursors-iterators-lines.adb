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
with VSS.Strings.Cursors.Markers.Internals;

package body VSS.Strings.Cursors.Iterators.Lines is

   function Terminator_First
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor;
   --  Return cursor of first character of the line terminator sequence.

   function Terminator_Last
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor;
   --  Return cursor of last character of the line terminator sequence.

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Line_Iterator) is
   begin
      Abstract_Segment_Iterator (Self).Invalidate;

      Self.Terminator := (1, 0, 0);
   end Invalidate;

   ----------------------
   -- Terminator_First --
   ----------------------

   function Terminator_First
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor
   is
      Position : VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      if Self.Keep_Terminator then
         Position := Self.Terminator;

      else
         Position := Self.Last_Position;
         Success  := Self.Owner.Handler.Forward (Self.Owner.Data, Position);
      end if;

      return Position;
   end Terminator_First;

   --------------------------------------
   -- Terminator_First_Character_Index --
   --------------------------------------

   function Terminator_First_Character_Index
     (Self : Line_Iterator'Class)
      return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Count (Self.Terminator_First.Index);
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

   ----------------------------------
   -- Terminator_First_UTF8_Offset --
   ----------------------------------

   function Terminator_First_UTF8_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return First_UTF8_Offset (Self.Owner, Self.Terminator_First);
   end Terminator_First_UTF8_Offset;

   -----------------------------------
   -- Terminator_First_UTF16_Offset --
   -----------------------------------

   function Terminator_First_UTF16_Offset
     (Self : Line_Iterator'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return First_UTF16_Offset (Self.Owner, Self.Terminator_First);
   end Terminator_First_UTF16_Offset;

   ---------------------
   -- Terminator_Last --
   ---------------------

   function Terminator_Last
     (Self : Line_Iterator'Class) return VSS.Implementation.Strings.Cursor is
   begin
      return
        (if Self.Keep_Terminator
           then Self.Last_Position
           else Self.Terminator);
   end Terminator_Last;

   -------------------------------------
   -- Terminator_Last_Character_Index --
   -------------------------------------

   function Terminator_Last_Character_Index
     (Self : Line_Iterator'Class)
      return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Count (Self.Terminator_Last.Index);
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

end VSS.Strings.Cursors.Iterators.Lines;
