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

with VSS.Unicode;

package VSS.Strings.Cursors is

   pragma Preelaborate;

   type Abstract_Cursor is abstract tagged limited private;

   function Character_Index
     (Self : Abstract_Cursor'Class) return VSS.Strings.Character_Index;
   --  Returns index of the logical element's starting character.

   function UTF8_Offset
     (Self : Abstract_Cursor'Class)
      return VSS.Unicode.UTF8_Code_Unit_Index;
   --  Returns offset of the logical element's starting code unit in UTF-8
   --  encoding.

   function UTF16_Offset
     (Self : Abstract_Cursor'Class)
      return VSS.Unicode.UTF16_Code_Unit_Index;
   --  Returns offset of the logical element's starting code unit in UTF-16
   --  encoding.

private

   type Abstract_Cursor is abstract new Referal_Limited_Base with record
      Position : VSS.Implementation.Strings.Cursor;
   end record;

   overriding procedure Invalidate (Self : in out Abstract_Cursor);

end VSS.Strings.Cursors;
