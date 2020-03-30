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
--  API to access to string data using line:column indexing.

package Magic.Strings.Texts is

   pragma Preelaborate;

   type Line_Count is new Natural;
   subtype Line_Index is Line_Count range 1 .. Line_Count'Last;

   type Column_Count is new Grapheme_Count;
   subtype Column_Index is Column_Count range 1 .. Column_Count'Last;

   type Magic_Text is new Magic_String with null record;

end Magic.Strings.Texts;
