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

   function First_Character_Index
     (Self : Abstract_Cursor'Class) return VSS.Strings.Character_Index is
   begin
      return
        VSS.Strings.Character_Index
          (if Self.Is_Segment
           then Self.First_Position.Index
           else Self.Position.Index);
   end First_Character_Index;

   ------------------------
   -- First_UTF16_Offset --
   ------------------------

   function First_UTF16_Offset
     (Self : Abstract_Cursor'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        (if Self.Is_Segment
         then Self.First_Position.UTF16_Offset
         else Self.Position.UTF16_Offset);
   end First_UTF16_Offset;

   -----------------------
   -- First_UTF8_Offset --
   -----------------------

   function First_UTF8_Offset
     (Self : Abstract_Cursor'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        (if Self.Is_Segment
         then Self.First_Position.UTF8_Offset
         else Self.Position.UTF8_Offset);
   end First_UTF8_Offset;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Abstract_Character_Cursor) is
   begin
      Self.Position := (1, 0, 0);
   end Invalidate;

   --------------------------
   -- Last_Character_Index --
   --------------------------

   function Last_Character_Index
     (Self : Abstract_Cursor'Class) return VSS.Strings.Character_Index is
   begin
      return
        VSS.Strings.Character_Index
          (if Self.Is_Segment
           then Self.Last_Position.Index
           else Self.Position.Index);
   end Last_Character_Index;

   -----------------------
   -- Last_UTF16_Offset --
   -----------------------

   function Last_UTF16_Offset
     (Self : Abstract_Cursor'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return
        (if Self.Is_Segment
         then Self.Last_Position.UTF16_Offset
         else Self.Position.UTF16_Offset);
   end Last_UTF16_Offset;

   ----------------------
   -- Last_UTF8_Offset --
   ----------------------

   function Last_UTF8_Offset
     (Self : Abstract_Cursor'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return
        (if Self.Is_Segment
         then Self.Last_Position.UTF8_Offset
         else Self.Position.UTF8_Offset);
   end Last_UTF8_Offset;

   ------------------
   -- UTF16_Offset --
   ------------------

   function UTF16_Offset
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index is
   begin
      return Self.First_UTF16_Offset;
   end UTF16_Offset;

   -----------------
   -- UTF8_Offset --
   -----------------

   function UTF8_Offset
     (Self : Abstract_Character_Cursor'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index is
   begin
      return Self.First_UTF8_Offset;
   end UTF8_Offset;

end VSS.Strings.Cursors;
