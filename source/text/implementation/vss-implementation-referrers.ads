------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Finalization;

with VSS.Implementation.Strings;
limited with VSS.Strings;

package VSS.Implementation.Referrers is

   pragma Preelaborate;

   type Virtual_String_Access is access all VSS.Strings.Virtual_String'Class;

   type Magic_String_Base is tagged;

   type Magic_String_Access is access all Magic_String_Base'Class;

   ------------------
   -- Referal_Base --
   ------------------

   type Referal_Base is tagged;

   type Referal_Access is access all Referal_Base'Class;

   type Referal_Base is abstract new Ada.Finalization.Controlled with record
      Owner    : Magic_String_Access;
      Next     : Referal_Access;
      Previous : Referal_Access;
   end record;

   procedure Connect
     (Self  : in out Referal_Base'Class;
      Owner : not null Magic_String_Access);
   --  Connect referal to string object

   procedure Disconnect (Self  : in out Referal_Base'Class);
   --  Disconnect referel from string object

   procedure Invalidate (Self : in out Referal_Base) is abstract;

   procedure String_Modified
     (Self     : in out Referal_Base;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is abstract;
   --  Called when referenced string object has been modified. Start is the
   --  position of the first character of the operation, it is state before
   --  modification operation, thus negative UTF* offset is not valid.
   --  Removed and Inserted parameters are sizes of the removed and inserted
   --  segments. All their members are valid.

   overriding procedure Adjust (Self : in out Referal_Base);
   --  Connect new object to the string object.

   overriding procedure Finalize (Self : in out Referal_Base);
   --  Invalidate referal state and disconnect from the string object.

   --------------------------
   -- Referal_Limited_Base --
   --------------------------

   type Referal_Limited_Base is tagged;

   type Referal_Limited_Access is access all Referal_Limited_Base'Class;

   type Referal_Limited_Base is
     abstract limited new Ada.Finalization.Limited_Controlled with record
      Owner    : Magic_String_Access;
      Next     : Referal_Limited_Access;
      Previous : Referal_Limited_Access;
   end record;

   procedure Connect
     (Self  : in out Referal_Limited_Base'Class;
      Owner : not null Magic_String_Access);
   --  Connect referal to string object

   procedure Disconnect (Self  : in out Referal_Limited_Base'Class);
   --  Disconnect referel from string object

   procedure Invalidate (Self : in out Referal_Limited_Base) is abstract;

   procedure String_Modified
     (Self     : in out Referal_Limited_Base;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is abstract;
   --  Called when referenced string object has been modified. Start is the
   --  position of the first character of the operation, it is state before
   --  modification operation, thus negative UTF* offset is not valid.
   --  Removed and Inserted parameters are sizes of the removed and inserted
   --  segments. All their members are valid.

   function Get_Owner
     (Self : Referal_Limited_Base'Class) return Virtual_String_Access
        with Inline;
   --  Return owner as access to Virtual_String object.

   overriding procedure Finalize (Self : in out Referal_Limited_Base);
   --  Invalidate referal state and disconnect from the string object.

   procedure Connect
     (Self  : in out Referal_Limited_Base'Class;
      Owner : aliased VSS.Strings.Virtual_String'Class);
   --  Connect referal to string object

   -----------------------
   -- Magic_String_Base --
   -----------------------

   type Magic_String_Base is
     abstract new Ada.Finalization.Controlled with record
      Limited_Head : Referal_Limited_Access;
      Limited_Tail : Referal_Limited_Access;
      Head         : Referal_Access;
      Tail         : Referal_Access;
   end record;

   --  overriding procedure Adjust (Self : in out Magic_String_Base);

   overriding procedure Finalize (Self : in out Magic_String_Base);

   procedure Notify_String_Modified
     (Self     : in out Magic_String_Base'Class;
      From     : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);
   --  Do notification about modification of the string. If some notification
   --  handler raises exception it is stored, and notification continued.
   --  First stored exception will be reraised before exit, thus call to this
   --  subprogram should be done at the end of the body of the caller
   --  subprogram or exception handling added to the caller subprogram.

end VSS.Implementation.Referrers;
