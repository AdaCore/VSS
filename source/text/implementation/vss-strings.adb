------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with Ada.Exceptions;

with VSS.Implementation.FNV_Hash;
with VSS.Implementation.String_Configuration;
with VSS.Implementation.String_Handlers;
with VSS.Strings.Cursors.Internals;
with VSS.Strings.Cursors.Iterators.Characters.Internals;
with VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals;
with VSS.Strings.Cursors.Iterators.Lines.Internals;
with VSS.String_Vectors.Internals;
with VSS.Strings.Texts;

package body VSS.Strings is

   procedure Notify_String_Modified
     (Self     : in out Virtual_String'Class;
      From     : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);
   --  Do notification about modification of the string. If some notification
   --  handler raises exception it is stored, and notification continued.
   --  First stored exception will be reraised before exit, thus call to this
   --  subprogram should be done at the end of the body of the caller
   --  subprogram or exception handling added to the caller subprogram.

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Handler (Left.Data).Is_Less
          (Left.Data,
           VSS.Implementation.Strings.Handler (Right.Data).all,
           Right.Data);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Handler (Left.Data).Is_Less_Or_Equal
          (Left.Data,
           VSS.Implementation.Strings.Handler (Right.Data).all,
           Right.Data);
   end "<=";

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return VSS.Implementation.Strings."=" (Left.Data, Right.Data);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return
        not VSS.Implementation.Strings.Handler (Left.Data).Is_Less_Or_Equal
          (Left.Data,
           VSS.Implementation.Strings.Handler (Right.Data).all,
           Right.Data);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return
        not VSS.Implementation.Strings.Handler (Left.Data).Is_Less
          (Left.Data,
           VSS.Implementation.Strings.Handler (Right.Data).all,
           Right.Data);
   end ">=";

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

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Virtual_String) is
   begin
      Self.Head := null;
      Self.Tail := null;
      Self.Limited_Head := null;
      Self.Limited_Tail := null;
      VSS.Implementation.Strings.Reference (Self.Data);
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Virtual_String'Class;
      Item : VSS.Characters.Virtual_Character)
   is
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      Start   : VSS.Implementation.Strings.Cursor;
      Offset  : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      Handler.After_Last_Character (Self.Data, Start);

      Handler.Append
        (Self.Data, VSS.Characters.Virtual_Character'Pos (Item), Offset);

      Notify_String_Modified (Self, Start, (0, 0, 0), Offset);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Virtual_String'Class;
      Item : Virtual_String'Class)
   is
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      Start   : VSS.Implementation.Strings.Cursor;
      Offset  : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      if Item.Is_Empty then
         return;
      end if;

      Handler.After_Last_Character (Self.Data, Start);

      Handler.Append (Self.Data, Item.Data, Offset);

      Notify_String_Modified (Self, Start, (0, 0, 0), Offset);
   end Append;

   ---------------
   -- Character --
   ---------------

   function Character
     (Self     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return
        VSS.Strings.Cursors.Iterators.Characters.Internals.Character
          (Self, Position);
   end Character;

   ----------------------
   -- Character_Length --
   ----------------------

   function Character_Length
     (Self : Virtual_String'Class) return Character_Count is
   begin
      return
        Character_Count
          (VSS.Implementation.Strings.Handler (Self.Data).Length (Self.Data));
   end Character_Length;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Virtual_String'Class) is
   begin
      VSS.Implementation.Strings.Unreference (Self.Data);
   end Clear;

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

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : in out Virtual_String'Class;
      From : VSS.Strings.Cursors.Abstract_Cursor'Class;
      To   : VSS.Strings.Cursors.Abstract_Cursor'Class)
   is
      use type VSS.Implementation.Strings.Character_Offset;

      Handler     :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      From_Cursor : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (From).all;
      To_Cursor   : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (To).all;
      Size        : VSS.Implementation.Strings.Cursor_Offset;

   begin
      if not VSS.Strings.Cursors.Internals.Is_Owner (From, Self)
        or else not VSS.Strings.Cursors.Internals.Is_Owner (To, Self)
      then
         return;
      end if;

      Handler.Compute_Size (Self.Data, From_Cursor, To_Cursor, Size);

      if Size.Index_Offset /= 0 then
         Handler.Delete (Self.Data, From_Cursor, Size);

         Self.Notify_String_Modified (From_Cursor, Size, (0, 0, 0));
      end if;
   end Delete;

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

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Self             : Virtual_String'Class;
      Suffix           : Virtual_String'Class;
      Case_Sensitivity : VSS.Strings.Case_Sensitivity := Case_Sensitive)
      return Boolean
   is
      pragma Unreferenced (Case_Sensitivity);

      use type VSS.Implementation.Strings.Character_Count;

      Self_Handler   :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      Suffix_Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Suffix.Data);

   begin
      if Self_Handler.Length (Self.Data)
           < Suffix_Handler.Length (Suffix.Data)
      then
         return False;

      else
         return
           Self_Handler.Ends_With
             (Self.Data, Suffix_Handler.all, Suffix.Data);
      end if;
   end Ends_With;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String) is
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

      --  Unreference shared data

      VSS.Implementation.Strings.Unreference (Self.Data);
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

   ---------------------
   -- First_Character --
   ---------------------

   function First_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return
        VSS.Strings.Cursors.Iterators.Characters.Internals.First_Character
          (Self);
   end First_Character;

   --------------------
   -- First_Grapheme --
   --------------------

   function First_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return
        VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals
          .First_Grapheme_Cluster (Self);
   end First_Grapheme_Cluster;

   ----------------
   -- First_Line --
   ----------------

   function First_Line
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator is
   begin
      return
        VSS.Strings.Cursors.Iterators.Lines.Internals.First_Line
          (Self, Terminators, Keep_Terminator);
   end First_Line;

   --------------
   -- Grapheme --
   --------------

   function Grapheme_Cluster
     (Self     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator
   is
      Start : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (Position).all;

   begin
      return
        VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals
          .Grapheme_Cluster (Self, Start);
   end Grapheme_Cluster;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Virtual_String'Class) return Hash_Type is
      Generator : VSS.Implementation.FNV_Hash.FNV_1a_Generator;

   begin
      VSS.Implementation.Strings.Handler
        (Self.Data).Hash (Self.Data, Generator);

      return
        VSS.Strings.Hash_Type (VSS.Implementation.FNV_Hash.Value (Generator));
   end Hash;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self     : in out Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Item     : VSS.Characters.Virtual_Character)
   is
      Start  : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (Position).all;
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      if not VSS.Strings.Cursors.Internals.Is_Owner (Position, Self) then
         return;
      end if;

      VSS.Implementation.Strings.Handler (Self.Data).Insert
        (Self.Data,
         Start,
         VSS.Characters.Virtual_Character'Pos (Item),
         Offset);

      Self.Notify_String_Modified (Start, (0, 0, 0), Offset);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self     : in out Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Item     : Virtual_String'Class)
   is
      Start  : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (Position).all;
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      if not VSS.Strings.Cursors.Internals.Is_Owner (Position, Self) then
         return;
      end if;

      VSS.Implementation.Strings.Handler (Self.Data).Insert
        (Self.Data, Start, Item.Data, Offset);

      Self.Notify_String_Modified (Start, (0, 0, 0), Offset);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Virtual_String'Class) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Handler (Self.Data).Is_Empty (Self.Data);
   end Is_Empty;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Virtual_String'Class) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Handler (Self.Data).Is_Null (Self.Data);
   end Is_Null;

   --------------------
   -- Last_Character --
   --------------------

   function Last_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return
        VSS.Strings.Cursors.Iterators.Characters.Internals.Last_Character
          (Self);
   end Last_Character;

   -------------------
   -- Last_Grapheme --
   -------------------

   function Last_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return
        VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals
          .Last_Grapheme_Cluster (Self);
   end Last_Grapheme_Cluster;

   ----------
   -- Line --
   ----------

   function Line
     (Self            : Virtual_String'Class;
      Position        : VSS.Strings.Cursors.Abstract_Character_Cursor'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator
   is
      Start : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (Position).all;

   begin
      return
        VSS.Strings.Cursors.Iterators.Lines.Internals.Line
          (Self, Start, Terminators, Keep_Terminator);
   end Line;

   ----------------------------
   -- Notify_String_Modified --
   ----------------------------

   procedure Notify_String_Modified
     (Self     : in out Virtual_String'Class;
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

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self : in out Virtual_String'Class;
      Item : VSS.Characters.Virtual_Character)
   is
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      VSS.Implementation.Strings.Handler (Self.Data).Insert
        (Self.Data,
         (1, 0, 0),
         VSS.Characters.Virtual_Character'Pos (Item),
         Offset);

      Notify_String_Modified (Self, (1, 0, 0), (0, 0, 0), Offset);
   end Prepend;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self : in out Virtual_String'Class;
      Item : Virtual_String'Class)
   is
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      VSS.Implementation.Strings.Handler (Self.Data).Insert
        (Self.Data, (1, 0, 0), Item.Data, Offset);

      Self.Notify_String_Modified ((1, 0, 0), (0, 0, 0), Offset);
   end Prepend;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Virtual_String) is
   begin
      raise Program_Error with "Not implemented";
   end Read;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self : in out Virtual_String'Class;
      From : VSS.Strings.Cursors.Abstract_Cursor'Class;
      To   : VSS.Strings.Cursors.Abstract_Cursor'Class;
      By   : VSS.Characters.Virtual_Character)
   is
      use type VSS.Implementation.Strings.Character_Offset;

      From_Cursor : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (From).all;
      To_Cursor   : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (To).all;
      Deleted     : VSS.Implementation.Strings.Cursor_Offset;
      Inserted    : VSS.Implementation.Strings.Cursor_Offset;

   begin
      if not VSS.Strings.Cursors.Internals.Is_Owner (From, Self)
        or else not VSS.Strings.Cursors.Internals.Is_Owner (To, Self)
      then
         return;
      end if;

      VSS.Implementation.Strings.Handler (Self.Data).Compute_Size
        (Self.Data, From_Cursor, To_Cursor, Deleted);

      if Deleted.Index_Offset /= 0 then
         VSS.Implementation.Strings.Handler (Self.Data).Delete
           (Self.Data, From_Cursor, Deleted);
      end if;

      VSS.Implementation.Strings.Handler (Self.Data).Insert
        (Self.Data,
         From_Cursor,
         VSS.Characters.Virtual_Character'Pos (By),
         Inserted);

      Self.Notify_String_Modified (From_Cursor, Deleted, Inserted);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self : in out Virtual_String'Class;
      From : VSS.Strings.Cursors.Abstract_Cursor'Class;
      To   : VSS.Strings.Cursors.Abstract_Cursor'Class;
      By   : Virtual_String'Class)
   is
      use type VSS.Implementation.Strings.Character_Offset;

      From_Cursor : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (From).all;
      To_Cursor   : constant VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (To).all;
      Deleted     : VSS.Implementation.Strings.Cursor_Offset;
      Inserted    : VSS.Implementation.Strings.Cursor_Offset;

   begin
      if not VSS.Strings.Cursors.Internals.Is_Owner (From, Self)
        or else not VSS.Strings.Cursors.Internals.Is_Owner (To, Self)
      then
         return;
      end if;

      VSS.Implementation.Strings.Handler (Self.Data).Compute_Size
        (Self.Data, From_Cursor, To_Cursor, Deleted);

      if Deleted.Index_Offset /= 0 then
         VSS.Implementation.Strings.Handler (Self.Data).Delete
           (Self.Data, From_Cursor, Deleted);
      end if;

      VSS.Implementation.Strings.Handler (Self.Data).Insert
        (Self.Data, From_Cursor, By.Data, Inserted);

      Self.Notify_String_Modified (From_Cursor, Deleted, Inserted);
   end Replace;

   -----------
   -- Slice --
   -----------

   function Slice
     (Self : Virtual_String'Class;
      From : VSS.Strings.Cursors.Abstract_Cursor'Class;
      To   : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return Virtual_String
   is
      First_Position :
        constant VSS.Strings.Cursors.Internals.Cursor_Constant_Access :=
          VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant (From);
      Last_Position  :
        constant VSS.Strings.Cursors.Internals.Cursor_Constant_Access :=
          VSS.Strings.Cursors.Internals.Last_Cursor_Access_Constant (To);

   begin
      return Result : Virtual_String do
         if VSS.Strings.Cursors.Internals.Is_Owner (From, Self)
           and then VSS.Strings.Cursors.Internals.Is_Owner (To, Self)
         then
            VSS.Implementation.Strings.Handler (Self.Data).Slice
              (Self.Data,
               First_Position.all,
               Last_Position.all,
               Result.Data);
         end if;
      end return;
   end Slice;

   -----------
   -- Slice --
   -----------

   function Slice
     (Self    : Virtual_String'Class;
      Segment : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return Virtual_String
   is
      First_Position :
        constant VSS.Strings.Cursors.Internals.Cursor_Constant_Access :=
          VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant (Segment);
      Last_Position  :
        constant VSS.Strings.Cursors.Internals.Cursor_Constant_Access :=
          VSS.Strings.Cursors.Internals.Last_Cursor_Access_Constant (Segment);

   begin
      return Result : Virtual_String do
         if VSS.Strings.Cursors.Internals.Is_Owner (Segment, Self) then
            VSS.Implementation.Strings.Handler (Self.Data).Slice
              (Self.Data,
               First_Position.all,
               Last_Position.all,
               Result.Data);
         end if;
      end return;
   end Slice;

   -----------------
   -- Split_Lines --
   -----------------

   function Split_Lines
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         VSS.Implementation.Strings.Handler (Self.Data).Split_Lines
           (Self.Data,
            Terminators,
            Keep_Terminator,
            VSS.String_Vectors.Internals.Data_Access (Result).all);
      end return;
   end Split_Lines;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With
     (Self             : Virtual_String'Class;
      Prefix           : Virtual_String'Class;
      Case_Sensitivity : VSS.Strings.Case_Sensitivity := Case_Sensitive)
      return Boolean
   is
      pragma Unreferenced (Case_Sensitivity);

      use type VSS.Implementation.Strings.Character_Count;

      Self_Handler   :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      Prefix_Handler :
        constant not null  VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Prefix.Data);

   begin
      if Self_Handler.Length (Self.Data)
           < Prefix_Handler.Length (Prefix.Data)
      then
         return False;

      else
         return
           Self_Handler.Starts_With
             (Self.Data, Prefix_Handler.all, Prefix.Data);
      end if;
   end Starts_With;

   ------------------
   -- To_Lowercase --
   ------------------

   function To_Lowercase (Self : Virtual_String'Class) return Virtual_String is
   begin
      return Result : Virtual_String do
         VSS.Implementation.Strings.Handler (Self.Data).Convert_Case
           (Self.Data,
            VSS.Implementation.String_Handlers.Lowercase,
            Result.Data);
      end return;
   end To_Lowercase;

   -------------------
   -- To_Magic_Text --
   -------------------

   function To_Magic_Text
     (Self : Virtual_String) return VSS.Strings.Texts.Magic_Text is
   begin
      return (Ada.Finalization.Controlled with
                Data         => <>,
                --  Data => (if Self.Data = null
                --           then null
                --           else Self.Data.To_Text),
                Head         => null,
                Tail         => null,
                Limited_Head => null,
                Limited_Tail => null);
   end To_Magic_Text;

   -------------------------
   -- To_Simple_Lowercase --
   -------------------------

   function To_Simple_Lowercase
     (Self : Virtual_String'Class) return Virtual_String is
   begin
      return Result : Virtual_String do
         VSS.Implementation.Strings.Handler (Self.Data).Convert_Case
           (Self.Data,
            VSS.Implementation.String_Handlers.Simple_Lowercase,
            Result.Data);
      end return;
   end To_Simple_Lowercase;

   -------------------------
   -- To_Simple_Titlecase --
   -------------------------

--   function To_Simple_Titlecase
--     (Self : Virtual_String'Class) return Virtual_String is
--   begin
--      return Result : Virtual_String do
--         VSS.Implementation.Strings.Handler (Self.Data).Convert_Case
--           (Self.Data,
--            VSS.Implementation.String_Handlers.Simple_Titlecase,
--            Result.Data);
--      end return;
--   end To_Simple_Titlecase;

   -------------------------
   -- To_Simple_Uppercase --
   -------------------------

   function To_Simple_Uppercase
     (Self : Virtual_String'Class) return Virtual_String is
   begin
      return Result : Virtual_String do
         VSS.Implementation.Strings.Handler (Self.Data).Convert_Case
           (Self.Data,
            VSS.Implementation.String_Handlers.Simple_Uppercase,
            Result.Data);
      end return;
   end To_Simple_Uppercase;

   ------------------
   -- To_Titlecase --
   ------------------

--   function To_Titlecase
--     (Self : Virtual_String'Class) return Virtual_String is
--   begin
--      return Result : Virtual_String do
--         VSS.Implementation.Strings.Handler (Self.Data).Convert_Case
--           (Self.Data,
--            VSS.Implementation.String_Handlers.Titlecase,
--            Result.Data);
--      end return;
--   end To_Titlecase;

   ------------------
   -- To_Uppercase --
   ------------------

   function To_Uppercase (Self : Virtual_String'Class) return Virtual_String is
   begin
      return Result : Virtual_String do
         VSS.Implementation.Strings.Handler (Self.Data).Convert_Case
           (Self.Data,
            VSS.Implementation.String_Handlers.Uppercase,
            Result.Data);
      end return;
   end To_Uppercase;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Wide_Wide_String) return Virtual_String
   is
      Success : Boolean;

   begin
      return Result : Virtual_String do
         --  First, attempt to place data in the storage inside the object of
         --  Magic_String type.

         VSS.Implementation.String_Configuration.In_Place_Handler
           .From_Wide_Wide_String
             (Item, Result.Data, Success);

         if not Success then
            --  Operation may fail for two reasons: source data is not
            --  well-formed UTF-8 or there is not enoght memory to store
            --  string in in-place storage.

            VSS.Implementation.String_Configuration.Default_Handler
              .From_Wide_Wide_String
                (Item, Result.Data, Success);
         end if;

         if not Success then
            raise Constraint_Error with "Ill-formed UTF-8 data";
         end if;
      end return;
   end To_Virtual_String;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Virtual_String) is
   begin
      raise Program_Error with "Not implemented";
   end Write;

end VSS.Strings;
