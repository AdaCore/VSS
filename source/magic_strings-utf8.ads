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

with Magic_Strings.Counted;

private package Magic_Strings.UTF8 is

   pragma Preelaborate;

   type Code_Unit is mod 256;

   type Code_Unit_Count is new Natural;

   type Code_Unit_Array is array (Code_Unit_Count range <>) of Code_Unit;

   type UTF8_Shared_String (Capacity : Code_Unit_Count) is
     new Magic_Strings.Counted.Abstract_Shared_String with record
      Data : Code_Unit_Array (0 .. Capacity);
      --  Buffer to store string's data. First unused code unit is set to
      --  zero, to allow to pass data to C.

      Size : Code_Unit_Count;
      --  Number of code units in the buffer.
   end record;

end Magic_Strings.UTF8;
