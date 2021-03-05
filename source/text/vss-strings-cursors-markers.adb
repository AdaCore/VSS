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

with VSS.Strings.Cursors.Markers.Internals;

package body VSS.Strings.Cursors.Markers is

   ---------------------------
   -- First_Character_Index --
   ---------------------------

   overriding function First_Character_Index
     (Self : Virtual_Marker) return VSS.Strings.Character_Index is
   begin
      return VSS.Strings.Character_Index (Self.Position.Index);
   end First_Character_Index;

   ------------------
   -- First_Marker --
   ------------------

   overriding function First_Marker
     (Self : Virtual_Marker)
      return VSS.Strings.Cursors.Markers.Virtual_Marker is
   begin
      return
        VSS.Strings.Cursors.Markers.Internals.New_Virtual_Marker
          (Self.Owner.all, Self.Position);
   end First_Marker;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   overriding function First_UTF16_Offset
     (Self : Virtual_Marker) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return Self.Position.UTF16_Offset;
   end First_UTF16_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   overriding function First_UTF8_Offset
     (Self : Virtual_Marker) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return Self.Position.UTF8_Offset;
   end First_UTF8_Offset;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Virtual_Marker) is
   begin
      Self.Position := (1, 0, 0);
   end Invalidate;

   --------------------------
   -- Last_Character_Index --
   --------------------------

   overriding function Last_Character_Index
     (Self : Virtual_Marker)
      return VSS.Strings.Character_Index renames First_Character_Index;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   overriding function Last_UTF16_Offset
     (Self : Virtual_Marker) return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Position.UTF16_Offset;
   end Last_UTF16_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   overriding function Last_UTF8_Offset
     (Self : Virtual_Marker) return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      raise Program_Error;
      --  Not implemented
      return Self.Position.UTF8_Offset;
   end Last_UTF8_Offset;

end VSS.Strings.Cursors.Markers;
