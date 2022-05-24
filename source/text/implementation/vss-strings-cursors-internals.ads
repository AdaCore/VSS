------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
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

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.Strings;

package VSS.Strings.Cursors.Internals is

   pragma Preelaborate;

   type Cursor_Constant_Access is
     access constant VSS.Implementation.Strings.Cursor;
   --  Access type to allow direct access to internal value of the cursor

   function First_Cursor_Access_Constant
     (Self : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return not null Cursor_Constant_Access;

   function Last_Cursor_Access_Constant
     (Self : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return not null Cursor_Constant_Access;

   function Is_Owner
     (Self  : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Owner : VSS.Strings.Virtual_String'Class) return Boolean;
   --  Return True when given string is owner of the cursor.

end VSS.Strings.Cursors.Internals;
