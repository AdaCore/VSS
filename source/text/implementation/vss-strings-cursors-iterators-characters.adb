--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.Strings.Cursors.Iterators.Characters is

   use type VSS.Implementation.Referrers.Magic_String_Access;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self : in out Character_Iterator) return Boolean
   is
      Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.UTF8_Strings.Backward (Text, Self.Position);
      end if;

      return False;
   end Backward;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Character_Iterator'Class)
      return VSS.Characters.Virtual_Character'Base
   is
      Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Magic_String_Access (Self.Owner).Data;

   begin
      if Self.Owner /= null then
         return
           VSS.Characters.Virtual_Character'Base'Val
             (VSS.Implementation.UTF8_Strings.Element (Text, Self.Position));
      end if;

      return
        VSS.Characters.Virtual_Character'Base'Val
          (VSS.Implementation.Strings.No_Character);
   end Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self : in out Character_Iterator) return Boolean is
   begin
      if Self.Owner = null then
         return False;
      end if;

      return
        VSS.Implementation.UTF8_Strings.Forward
          (VSS.Strings.Magic_String_Access (Self.Owner).Data, Self.Position);
   end Forward;

   -------------
   -- Forward --
   -------------

   function Forward
     (Self    : in out Character_Iterator;
      Element : out VSS.Characters.Virtual_Character'Base) return Boolean
   is
      Code   : VSS.Unicode.Code_Point'Base :=
        VSS.Implementation.Strings.No_Character;
      Result : Boolean := False;

   begin
      if Self.Owner /= null then
         Result :=
           VSS.Implementation.UTF8_Strings.Forward_Element
             (VSS.Strings.Magic_String_Access (Self.Owner).Data,
              Self.Position,
              Code);
      end if;

      Element := VSS.Characters.Virtual_Character'Base'Val (Code);

      return Result;
   end Forward;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Character_Iterator) return Boolean is
   begin
      if Self.Owner /= null then
         return
           VSS.Implementation.UTF8_Strings.Has_Character
             (VSS.Strings.Magic_String_Access (Self.Owner).Data,
              Self.Position);
      end if;

      return False;
   end Has_Element;

   --------------------
   -- Set_After_Last --
   --------------------

   procedure Set_After_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class) is
   begin
      Self.Reconnect (On'Unrestricted_Access);
      VSS.Implementation.UTF8_Strings.After_Last_Character
        (VSS.Strings.Magic_String_Access (Self.Owner).Data, Self.Position);
   end Set_After_Last;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (Self     : in out Character_Iterator;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
   is
      Cursor_Owner    : VSS.Implementation.Referrers.Magic_String_Access;
      Cursor_Position : VSS.Implementation.Strings.Cursor;

   begin
      Get_Owner_And_Position (Position, Cursor_Owner, Cursor_Position);

      Self.Reconnect (Cursor_Owner);
      Self.Position := Cursor_Position;
   end Set_At;

   ------------------
   -- Set_At_First --
   ------------------

   procedure Set_At_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Dummy : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);
      VSS.Implementation.UTF8_Strings.Before_First_Character
        (VSS.Strings.Magic_String_Access (Self.Owner).Data, Self.Position);
      Dummy :=
        VSS.Implementation.UTF8_Strings.Forward
          (VSS.Strings.Magic_String_Access (Self.Owner).Data, Self.Position);
   end Set_At_First;

   -----------------
   -- Set_At_Last --
   -----------------

   procedure Set_At_Last
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class)
   is
      Dummy : Boolean;

   begin
      Self.Reconnect (On'Unrestricted_Access);
      VSS.Implementation.UTF8_Strings.After_Last_Character
        (VSS.Strings.Magic_String_Access (Self.Owner).Data, Self.Position);
      Dummy :=
        VSS.Implementation.UTF8_Strings.Backward
          (VSS.Strings.Magic_String_Access (Self.Owner).Data, Self.Position);
   end Set_At_Last;

   ----------------------
   -- Set_Before_First --
   ----------------------

   procedure Set_Before_First
     (Self : in out Character_Iterator;
      On   : VSS.Strings.Virtual_String'Class) is
   begin
      Self.Reconnect (On'Unrestricted_Access);
      VSS.Implementation.UTF8_Strings.Before_First_Character
        (VSS.Strings.Magic_String_Access (Self.Owner).Data, Self.Position);
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
