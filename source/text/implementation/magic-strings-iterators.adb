------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body Magic.Strings.Iterators is

   ---------------------
   -- Character_Index --
   ---------------------

   function Character_Index
     (Self : Abstract_Iterator'Class) return Magic.Strings.Character_Index is
   begin
      return Self.Position.Index;
   end Character_Index;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Abstract_Iterator) is
   begin
      Self.Position := (1, 0, 0);
   end Invalidate;

   ------------------
   -- UTF16_Offset --
   ------------------

   function UTF16_Offset
     (Self : Abstract_Iterator'Class)
      return Magic.Unicode.UTF16_Code_Unit_Index is
   begin
      return Self.Position.UTF16_Offset;
   end UTF16_Offset;

   -----------------
   -- UTF8_Offset --
   -----------------

   function UTF8_Offset
     (Self : Abstract_Iterator'Class)
      return Magic.Unicode.UTF8_Code_Unit_Index is
   begin
      return Self.Position.UTF8_Offset;
   end UTF8_Offset;

end Magic.Strings.Iterators;
