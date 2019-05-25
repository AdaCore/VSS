------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
--  Generic implementation of the string which use UTF-8 encoding for data.

package body Magic_Strings.UTF8 is

   type UTF8_Segment_Access is access all UTF8_Segment;

   -----------------
   -- From_String --
   -----------------

   procedure From_UTF_8_String
     (Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Segment : out String_Access;
      Success : out Boolean)
   is
      Aux : UTF8_Segment_Access;

   begin
      if Item'Length = 0 then
         Segment := null;
         Success := True;

         return;
      end if;

      Aux :=
        new UTF8_Segment (UTF.UTF8_Code_Unit_Count (Item'Length + 1));

      for J in Item'Range loop
         --  XXX Verification of the UTF-8 format is not implemented.

         Aux.Data (UTF.UTF8_Code_Unit_Count (J - Item'First)) :=
           Character'Pos (Item (J));
      end loop;

      Success := True;
      Segment := String_Access (Aux);
   end From_UTF_8_String;

end Magic_Strings.UTF8;
