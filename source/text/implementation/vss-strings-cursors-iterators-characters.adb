--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Handlers;

package body VSS.Strings.Cursors.Iterators.Characters is

   use type VSS.Implementation.Referrers.Magic_String_Access;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self : in out Character_Iterator) return Boolean
   is
      Data : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.Strings.Handler
             (Data).Backward (Data, Self.Position);
      end if;

      return False;
   end Backward;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Character_Iterator'Class)
      return VSS.Characters.Virtual_Character
   is
      Data : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner /= null then
         return
           VSS.Characters.Virtual_Character'Val
             (VSS.Implementation.Strings.Handler
                (Data).Element (Data, Self.Position));
      end if;

      return VSS.Characters.Virtual_Character'Val (16#00_0000#);
   end Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self : in out Character_Iterator) return Boolean
   is
      Data : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.Strings.Handler
             (Data).Forward (Data, Self.Position);
      end if;

      return False;
   end Forward;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Character_Iterator) return Boolean
   is
      Data : VSS.Implementation.Strings.String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.Strings.Handler
             (Data).Has_Character (Data, Self.Position);
      end if;

      return False;
   end Has_Element;

   --------------------
   -- Set_After_Last --
   --------------------

   procedure Set_After_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (On.Data);

   begin
      if Self.Owner /= On'Unrestricted_Access then
         Self.Disconnect;
         Self.Connect (On'Unrestricted_Access);
      end if;

      Handler.After_Last_Character (On.Data, Self.Position);
   end Set_After_Last;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (Self     : in out Character_Iterator;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class) is
   begin
      if Position in Character_Cursor_Limited_Base'Class then
         declare
            P : Character_Cursor_Limited_Base'Class
              renames Character_Cursor_Limited_Base'Class (Position);

         begin
            if Self.Owner /= P.Owner then
               if Self.Owner /= null then
                  Self.Disconnect;
               end if;

               if P.Owner /= null then
                  Self.Connect (P.Owner);
               end if;
            end if;

            Self.Position := P.Position;
         end;

      elsif Position in Character_Cursor_Base'Class then
         declare
            P : Character_Cursor_Base'Class
              renames Character_Cursor_Base'Class (Position);

         begin
            if Self.Owner /= P.Owner then
               if Self.Owner /= null then
                  Self.Disconnect;
               end if;

               if P.Owner /= null then
                  Self.Connect (P.Owner);
               end if;
            end if;

            Self.Position := P.Position;
         end;

      else
         raise Program_Error;
      end if;
   end Set_At;

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

   ----------------------
   -- Set_Before_First --
   ----------------------

   procedure Set_Before_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (On.Data);

   begin
      if Self.Owner /= On'Unrestricted_Access then
         Self.Disconnect;
         Self.Connect (On'Unrestricted_Access);
      end if;

      Handler.Before_First_Character (On.Data, Self.Position);
   end Set_Before_First;

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
