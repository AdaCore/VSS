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

with VSS.Implementation.String_Handlers;

package body VSS.Strings.Cursors.Iterators.Characters is

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self : in out Character_Iterator) return Boolean is
   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.Strings.Handler
             (Self.Owner.Data).Backward (Self.Owner.Data, Self.Position);
      end if;

      return False;
   end Backward;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Character_Iterator'Class)
      return VSS.Characters.Virtual_Character is
   begin
      if Self.Owner /= null then
         return
           VSS.Characters.Virtual_Character'Val
             (VSS.Implementation.Strings.Handler
                (Self.Owner.Data).Element (Self.Owner.Data, Self.Position));
      end if;

      return VSS.Characters.Virtual_Character'Val (16#00_0000#);
   end Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self : in out Character_Iterator) return Boolean is
   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.Strings.Handler
             (Self.Owner.Data).Forward (Self.Owner.Data, Self.Position);
      end if;

      return False;
   end Forward;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Character_Iterator) return Boolean is
   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.Strings.Handler
             (Self.Owner.Data).Has_Character (Self.Owner.Data, Self.Position);
      end if;

      return False;
   end Has_Element;

   ------------------
   -- Set_At_First --
   ------------------

   procedure Set_At_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (On.Data);
      Dummy   : Boolean;

   begin
      if Self.Owner /= On'Unrestricted_Access then
         Self.Disconnect;
         Self.Connect (On'Unrestricted_Access);
      end if;

      Handler.Before_First_Character (On.Data, Self.Position);
      Dummy := Handler.Forward (On.Data, Self.Position);
   end Set_At_First;

   -----------------
   -- Set_At_Last --
   -----------------

   procedure Set_At_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (On.Data);
      Dummy   : Boolean;

   begin
      if Self.Owner /= On'Unrestricted_Access then
         Self.Disconnect;
         Self.Connect (On'Unrestricted_Access);
      end if;

      Handler.After_Last_Character (On.Data, Self.Position);
      Dummy := Handler.Backward (On.Data, Self.Position);
   end Set_At_Last;

   ---------------------
   -- String_Modified --
   ---------------------

   overriding procedure String_Modified
     (Self     : in out Character_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Deleted  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      if VSS.Implementation.Strings.Fixup_Delete
           (Self.Position, Start, Deleted)
      then
         VSS.Implementation.Strings.Fixup_Insert
           (Self.Position, Start, Inserted);

      else
         Self.Invalidate;
         Self.Disconnect;
      end if;
   end String_Modified;

end VSS.Strings.Cursors.Iterators.Characters;
