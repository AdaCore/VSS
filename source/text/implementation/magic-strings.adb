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

with Magic.Strings.Iterators.Characters.Internals;
with Magic.Strings.Texts;

package body Magic.Strings is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Magic_String) is
      Aux : String_Access;

   begin
      if Self.Data /= null then
         Aux := Self.Data.Reference;

         if Aux /= Self.Data then
            Self.Data := Aux;
         end if;
      end if;
   end Adjust;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self  : in out Referal_Limited_Base'Class;
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

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Self : in out Referal_Limited_Base'Class) is
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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Magic_String) is
   begin
      --  Invalidate and disconnect all referals

      while Self.Head /= null loop
         Self.Head.Invalidate;
         Self.Head.Disconnect;
      end loop;

      --  Unreference shared data

      if Self.Data /= null then
         Self.Data.Unreference;
         Self.Data := null;
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

   ---------------------
   -- First_Character --
   ---------------------

   function First_Character
     (Self : Magic_String'Class)
      return Magic.Strings.Iterators.Characters.Character_Iterator is
   begin
      return
        Magic.Strings.Iterators.Characters.Internals.First_Character (Self);
   end First_Character;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Magic_String'Class) return Boolean is
   begin
      return Self.Data = null or else Self.Data.Is_Empty;
   end Is_Empty;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Magic_String'Class) return Boolean is
   begin
      return Self.Data = null;
   end Is_Null;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Magic_String) is
   begin
      raise Program_Error with "Not implemented";
   end Read;

   -------------------
   -- To_Magic_Text --
   -------------------

   function To_Magic_Text
     (Self : Magic_String) return Magic.Strings.Texts.Magic_Text is
   begin
      return (Ada.Finalization.Controlled with
                Data => (if Self.Data = null
                         then null
                         else Self.Data.To_Text),
                Head => null,
                Tail => null);
   end To_Magic_Text;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Magic_String) is
   begin
      raise Program_Error with "Not implemented";
   end Write;

end Magic.Strings;
