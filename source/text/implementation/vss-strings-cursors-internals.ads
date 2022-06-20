--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
