--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
      Owner : VSS.Strings.Virtual_String'Class) return Boolean
   is
      use type VSS.Implementation.Referrers.Virtual_String_Access;

   begin
      if Self in VSS.Implementation.Referrers.Referal_Base'Class then
         return
           VSS.Implementation.Referrers.Referal_Base'Class (Self).Get_Owner
             = Owner'Unrestricted_Access;

      elsif Self
              in VSS.Implementation.Referrers.Referal_Limited_Base'Class
      then
         return
           VSS.Implementation.Referrers.Referal_Limited_Base'Class
             (Self).Get_Owner = Owner'Unrestricted_Access;

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
