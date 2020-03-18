------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

package body Magic.Strings.Slices is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Slice_Shared_String) is
   begin
      if Self.Data /= null then
         Self.Data.Unreference;
         Self.Data := null;
      end if;
   end Finalize;

   ---------------------
   -- First_Character --
   ---------------------

   overriding procedure First_Character
     (Self     : Slice_Shared_String;
      Position : in out Cursor) is
   begin
      null;
   end First_Character;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : Slice_Shared_String;
      Position : in out Cursor) return Boolean is
   begin
      return False;
   end Forward;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : Slice_Shared_String) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Is_Empty;

   -------------
   -- To_Text --
   -------------

   overriding function To_Text
     (Self : in out Slice_Shared_String) return String_Access is
   begin
      --  XXX Not implemented

      return null;
   end To_Text;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : Slice_Shared_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return "";
   end To_UTF_8_String;

end Magic.Strings.Slices;
