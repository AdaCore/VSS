------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

package body VSS.Strings.Cursors.Internals is

   ----------------------------------
   -- First_Cursor_Access_Constant --
   ----------------------------------

   function First_Cursor_Access_Constant
     (Self : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return not null Cursor_Constant_Access is
   begin
      if Self in VSS.Strings.Cursors.Character_Cursor_Limited_Base'Class then
         return
           VSS.Strings.Cursors.Character_Cursor_Limited_Base
             (Self).Position'Unchecked_Access;

      elsif Self in VSS.Strings.Cursors.Character_Cursor_Base'Class then
         return
           VSS.Strings.Cursors.Character_Cursor_Base
             (Self).Position'Unchecked_Access;

      elsif Self in VSS.Strings.Cursors.Segment_Cursor_Limited_Base'Class then
         return
           VSS.Strings.Cursors.Segment_Cursor_Limited_Base
             (Self).First_Position'Unchecked_Access;

      else
         raise Program_Error;
      end if;
   end First_Cursor_Access_Constant;

   --------------
   -- Is_Owner --
   --------------

   function Is_Owner
     (Self  : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Owner : VSS.Strings.Virtual_String'Class) return Boolean is
   begin
      if Self in VSS.Strings.Cursors.Character_Cursor_Limited_Base'Class then
         return
           VSS.Strings.Cursors.Character_Cursor_Limited_Base (Self).Owner
             = Owner'Unrestricted_Access;

      elsif Self in VSS.Strings.Cursors.Character_Cursor_Base'Class then
         return
           VSS.Strings.Cursors.Character_Cursor_Base'Class (Self).Owner
             = Owner'Unrestricted_Access;

      elsif Self in VSS.Strings.Cursors.Segment_Cursor_Limited_Base'Class then
         return
           VSS.Strings.Cursors.Segment_Cursor_Limited_Base'Class (Self).Owner
             = Owner'Unrestricted_Access;

      else
         raise Program_Error;
      end if;
   end Is_Owner;

   ---------------------------------
   -- Last_Cursor_Access_Constant --
   ---------------------------------

   function Last_Cursor_Access_Constant
     (Self : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return not null Cursor_Constant_Access is
   begin
      if Self in VSS.Strings.Cursors.Character_Cursor_Limited_Base'Class then
         return
           VSS.Strings.Cursors.Character_Cursor_Limited_Base
             (Self).Position'Unchecked_Access;

      elsif Self in VSS.Strings.Cursors.Character_Cursor_Base'Class then
         return
           VSS.Strings.Cursors.Character_Cursor_Base
             (Self).Position'Unchecked_Access;

      elsif Self in VSS.Strings.Cursors.Segment_Cursor_Limited_Base'Class then
         return
           VSS.Strings.Cursors.Segment_Cursor_Limited_Base
             (Self).Last_Position'Unchecked_Access;

      else
         raise Program_Error;
      end if;
   end Last_Cursor_Access_Constant;

end VSS.Strings.Cursors.Internals;
