--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;

with VSS.Strings.Internals;

package body VSS.Implementation.Referrers is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Referal_Base) is
      Owner : constant Magic_String_Access := Self.Owner;

   begin
      Self.Owner    := null;
      Self.Next     := null;
      Self.Previous := null;

      if Owner /= null then
         Self.Connect (Owner);
      end if;
   end Adjust;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self  : in out Referal_Base'Class;
      Owner : not null Magic_String_Access) is
   begin
      if Owner.Head = null then
         Owner.Head := Self'Unchecked_Access;
         Owner.Tail := Self'Unchecked_Access;

      else
         Owner.Tail.Next := Self'Unchecked_Access;
         Self.Previous := Owner.Tail;
         Owner.Tail := Self'Unchecked_Access;
      end if;

      Self.Owner := Owner;
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self  : in out Referal_Limited_Base'Class;
      Owner : not null Magic_String_Access) is
   begin
      if Owner.Limited_Head = null then
         Owner.Limited_Head := Self'Unchecked_Access;
         Owner.Limited_Tail := Self'Unchecked_Access;

      else
         Owner.Limited_Tail.Next := Self'Unchecked_Access;
         Self.Previous := Owner.Limited_Tail;
         Owner.Limited_Tail := Self'Unchecked_Access;
      end if;

      Self.Owner := Owner;
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self  : in out Referal_Limited_Base'Class;
      Owner : aliased VSS.Strings.Virtual_String'Class) is
   begin
      Self.Connect
        (VSS.Strings.Internals.To_Magic_String_Access
           (Owner'Unrestricted_Access));
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Self : in out Referal_Base'Class) is
   begin
      if Self.Owner /= null then
         if Self.Owner.Head = Self'Unchecked_Access then
            Self.Owner.Head := Self.Owner.Head.Next;
         end if;

         if Self.Owner.Tail = Self'Unchecked_Access then
            Self.Owner.Tail := Self.Owner.Tail.Previous;
         end if;

         if Self.Previous /= null then
            Self.Previous.Next := Self.Next;
         end if;

         if Self.Next /= null then
            Self.Next.Previous := Self.Previous;
         end if;

         Self.Owner    := null;
         Self.Previous := null;
         Self.Next     := null;
      end if;
   end Disconnect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Self : in out Referal_Limited_Base'Class) is
   begin
      if Self.Owner /= null then
         if Self.Owner.Limited_Head = Self'Unchecked_Access then
            Self.Owner.Limited_Head := Self.Owner.Limited_Head.Next;
         end if;

         if Self.Owner.Limited_Tail = Self'Unchecked_Access then
            Self.Owner.Limited_Tail := Self.Owner.Limited_Tail.Previous;
         end if;

         if Self.Previous /= null then
            Self.Previous.Next := Self.Next;
         end if;

         if Self.Next /= null then
            Self.Next.Previous := Self.Previous;
         end if;

         Self.Owner    := null;
         Self.Previous := null;
         Self.Next     := null;
      end if;
   end Disconnect;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Magic_String_Base) is
   begin
      --  Invalidate and disconnect all referals

      while Self.Head /= null loop
         Self.Head.Invalidate;
         Self.Head.Disconnect;
      end loop;

      while Self.Limited_Head /= null loop
         Self.Limited_Head.Invalidate;
         Self.Limited_Head.Disconnect;
      end loop;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Referal_Base) is
   begin
      if Self.Owner /= null then
         Referal_Base'Class (Self).Invalidate;
         Self.Disconnect;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Referal_Limited_Base) is
   begin
      if Self.Owner /= null then
         Referal_Limited_Base'Class (Self).Invalidate;
         Self.Disconnect;
      end if;
   end Finalize;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
     (Self : Referal_Base'Class) return Virtual_String_Access is
   begin
      return VSS.Strings.Internals.To_Virtual_String_Access (Self.Owner);
   end Get_Owner;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
     (Self : Referal_Limited_Base'Class) return Virtual_String_Access is
   begin
      return VSS.Strings.Internals.To_Virtual_String_Access (Self.Owner);
   end Get_Owner;

   ----------------------------
   -- Notify_String_Modified --
   ----------------------------

   procedure Notify_String_Modified
     (Self     : in out Magic_String_Base'Class;
      From     : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset)
   is
      use type Ada.Exceptions.Exception_Id;

      Occurrence : Ada.Exceptions.Exception_Occurrence;

   begin
      declare
         Current : Referal_Limited_Access := Self.Limited_Head;
         Next    : Referal_Limited_Access;

      begin
         while Current /= null loop
            Next := Current.Next;

            begin
               Current.String_Modified (From, Removed, Inserted);

            exception
               when X : others =>
                  if Ada.Exceptions.Exception_Identity (Occurrence)
                    = Ada.Exceptions.Null_Id
                  then
                     --  Save first raised exception only.

                     Ada.Exceptions.Save_Occurrence (Occurrence, X);
                  end if;
            end;

            Current := Next;
         end loop;
      end;

      declare
         Current : Referal_Access := Self.Head;
         Next    : Referal_Access;

      begin
         while Current /= null loop
            Next := Current.Next;

            begin
               Current.String_Modified (From, Removed, Inserted);

            exception
               when X : others =>
                  if Ada.Exceptions.Exception_Identity (Occurrence)
                    = Ada.Exceptions.Null_Id
                  then
                     --  Save first raised exception only.

                     Ada.Exceptions.Save_Occurrence (Occurrence, X);
                  end if;
            end;

            Current := Next;
         end loop;
      end;

      Ada.Exceptions.Reraise_Occurrence (Occurrence);
   end Notify_String_Modified;

end VSS.Implementation.Referrers;
