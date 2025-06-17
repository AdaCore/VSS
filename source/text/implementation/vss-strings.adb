--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Character_Codes;
with VSS.Implementation.FNV_Hash;
with VSS.Implementation.Line_Terminator;
with VSS.Implementation.Text_Handlers;
with VSS.Strings.Cursors.Internals;
with VSS.Strings.Cursors.Iterators.Characters;
with VSS.Strings.Cursors.Iterators.Grapheme_Clusters;
with VSS.Strings.Cursors.Iterators.Lines;
with VSS.Strings.Cursors.Iterators.Words;
with VSS.String_Vectors.Internals;
with VSS.Transformers;
with VSS.Unicode;

package body VSS.Strings is

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : Virtual_String;
      Right : Virtual_String) return Virtual_String is
   begin
      return Result : Virtual_String do
         declare
            Offset : VSS.Implementation.Strings.Cursor_Offset;

         begin
            Result.Data := Left.Data;
            VSS.Implementation.Strings.Reference (Result.Data);
            VSS.Implementation.Strings.Variable_Handler
              (Result.Data).Append (Result.Data, Right.Data, Offset);
         end;
      end return;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : Virtual_String;
      Right : VSS.Characters.Virtual_Character) return Virtual_String is
   begin
      return Result : Virtual_String do
         declare
            Offset : VSS.Implementation.Strings.Cursor_Offset;

         begin
            Result.Data := Left.Data;
            VSS.Implementation.Strings.Reference (Result.Data);
            VSS.Implementation.Strings.Variable_Handler (Result.Data).Append
              (VSS.Characters.Virtual_Character'Pos (Right),
               Offset);
         end;
      end return;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : VSS.Characters.Virtual_Character;
      Right : Virtual_String) return Virtual_String is
   begin
      return Result : Virtual_String do
         declare
            Offset : VSS.Implementation.Strings.Cursor_Offset;

         begin
            VSS.Implementation.Strings.Variable_Handler (Result.Data).Append
              (VSS.Characters.Virtual_Character'Pos (Left),
               Offset);
            VSS.Implementation.Strings.Variable_Handler (Result.Data).Append
              (Result.Data, Right.Data, Offset);
         end;
      end return;
   end "&";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Character_Count;
      Right : VSS.Characters.Virtual_Character) return Virtual_String is
   begin
      return Result : Virtual_String do
         --  Result.Set_Capacity (Left);

         for J in 1 .. Left loop
            Result.Append (Right);
         end loop;
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Virtual_String)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. Left loop
            Result.Append (Right);
         end loop;
      end return;
   end "*";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Constant_Handler (Left.Data).Is_Less
          (VSS.Implementation.Strings.Constant_Handler (Right.Data).all);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Constant_Handler
          (Left.Data).Is_Less_Or_Equal
             (VSS.Implementation.Strings.Constant_Handler (Right.Data).all);
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
        not VSS.Implementation.Strings.Constant_Handler
          (Left.Data).Is_Less_Or_Equal
             (VSS.Implementation.Strings.Constant_Handler (Right.Data).all);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : Virtual_String;
      Right : Virtual_String) return Boolean is
   begin
      return
        not VSS.Implementation.Strings.Constant_Handler (Left.Data).Is_Less
          (VSS.Implementation.Strings.Constant_Handler (Right.Data).all);
   end ">=";

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

   --------------------------
   -- After_Last_Character --
   --------------------------

   function After_Last_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
      do
         Result.Set_After_Last (Self);
      end return;
   end After_Last_Character;

   ---------------------------------
   -- After_Last_Grapheme_Cluster --
   ---------------------------------

   function After_Last_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Grapheme_Clusters
                 .Grapheme_Cluster_Iterator
      do
         Result.Set_After_Last (Self);
      end return;
   end After_Last_Grapheme_Cluster;

   ---------------------
   -- After_Last_Word --
   ---------------------

   function After_Last_Word
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Words.Word_Iterator do
         Result.Set_After_Last (Self);
      end return;
   end After_Last_Word;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Virtual_String'Class;
      Item : VSS.Characters.Virtual_Character)
   is
      Text : constant not null
        VSS.Implementation.Strings.Variable_Text_Handler_Access :=
          VSS.Implementation.Strings.Variable_Handler (Self.Data);
      Start   : VSS.Implementation.Strings.Cursor;
      Offset  : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      Text.After_Last_Character (Start);
      Text.Append (VSS.Characters.Virtual_Character'Pos (Item), Offset);

      Self.Notify_String_Modified (Start, (0, 0, 0), Offset);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Virtual_String'Class;
      Item : Virtual_String'Class)
   is
      Text    : constant not null
        VSS.Implementation.Strings.Variable_Text_Handler_Access :=
          VSS.Implementation.Strings.Variable_Handler (Self.Data);
      Start   : VSS.Implementation.Strings.Cursor;
      Offset  : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      if Item.Is_Empty then
         return;
      end if;

      Text.After_Last_Character (Start);
      Text.Append (Self.Data, Item.Data, Offset);

      Self.Notify_String_Modified (Start, (0, 0, 0), Offset);
   end Append;

   ------------------
   -- At_Character --
   ------------------

   function At_Character
     (Self     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
      do
         if VSS.Strings.Cursors.Internals.Is_Owner (Position, Self) then
            Result.Set_At (Position);
         end if;
      end return;
   end At_Character;

   ------------------------
   -- At_First_Character --
   ------------------------

   function At_First_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
      do
         Result.Set_At_First (Self);
      end return;
   end At_First_Character;

   -------------------------------
   -- At_First_Grapheme_Cluster --
   -------------------------------

   function At_First_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Grapheme_Clusters
                 .Grapheme_Cluster_Iterator
      do
         Result.Set_At_First (Self);
      end return;
   end At_First_Grapheme_Cluster;

   -------------------
   -- At_First_Line --
   -------------------

   function At_First_Line
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Lines.Line_Iterator do
         Result.Set_At_First (Self, Terminators, Keep_Terminator);
      end return;
   end At_First_Line;
   -------------------
   -- At_First_Word --
   -------------------

   function At_First_Word
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Words.Word_Iterator do
         Result.Set_At_First (Self);
      end return;
   end At_First_Word;

   -------------------------
   -- At_Grapheme_Cluster --
   -------------------------

   function At_Grapheme_Cluster
     (Self     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Grapheme_Clusters
                 .Grapheme_Cluster_Iterator
      do
         if VSS.Strings.Cursors.Internals.Is_Owner (Position, Self) then
            Result.Set_At (Position);
         end if;
      end return;
   end At_Grapheme_Cluster;

   -----------------------
   -- At_Last_Character --
   -----------------------

   function At_Last_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
      do
         Result.Set_At_Last (Self);
      end return;
   end At_Last_Character;

   ------------------------------
   -- At_Last_Grapheme_Cluster --
   ------------------------------

   function At_Last_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Grapheme_Clusters
                 .Grapheme_Cluster_Iterator
      do
         Result.Set_At_Last (Self);
      end return;
   end At_Last_Grapheme_Cluster;

   ------------------
   -- At_Last_Line --
   ------------------

   function At_Last_Line
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Lines.Line_Iterator do
         Result.Set_At_Last (Self, Terminators, Keep_Terminator);
      end return;
   end At_Last_Line;

   ------------------
   -- At_Last_Word --
   ------------------

   function At_Last_Word
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Words.Word_Iterator do
         Result.Set_At_Last (Self);
      end return;
   end At_Last_Word;

   -------------
   -- At_Line --
   -------------

   function At_Line
     (Self            : Virtual_String'Class;
      Position        : VSS.Strings.Cursors.Abstract_Character_Cursor'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Lines.Line_Iterator do
         if VSS.Strings.Cursors.Internals.Is_Owner (Position, Self) then
            Result.Set_At (Position, Terminators, Keep_Terminator);
         end if;
      end return;
   end At_Line;

   -------------
   -- At_Word --
   -------------

   function At_Word
     (Self     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Words.Word_Iterator do
         if VSS.Strings.Cursors.Internals.Is_Owner (Position, Self) then
            Result.Set_At (Position);
         end if;
      end return;
   end At_Word;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   function Before_First_Character
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Characters.Character_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Characters.Character_Iterator
      do
         Result.Set_Before_First (Self);
      end return;
   end Before_First_Character;

   -----------------------------------
   -- Before_First_Grapheme_Cluster --
   -----------------------------------

   function Before_First_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return Result :
               VSS.Strings.Cursors.Iterators.Grapheme_Clusters
                 .Grapheme_Cluster_Iterator
      do
         Result.Set_Before_First (Self);
      end return;
   end Before_First_Grapheme_Cluster;

   -----------------------
   -- Before_First_Word --
   -----------------------

   function Before_First_Word
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator is
   begin
      return Result : VSS.Strings.Cursors.Iterators.Words.Word_Iterator do
         Result.Set_Before_First (Self);
      end return;
   end Before_First_Word;

   ----------------------
   -- Character_Length --
   ----------------------

   function Character_Length
     (Self : Virtual_String'Class) return Character_Count is
   begin
      return
        Character_Count
          (VSS.Implementation.Strings.Constant_Handler (Self.Data).Length);
   end Character_Length;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Virtual_String'Class) is
   begin
      VSS.Implementation.Strings.Unreference (Self.Data);
   end Clear;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : in out Virtual_String'Class;
      From : VSS.Strings.Cursors.Abstract_Cursor'Class;
      To   : VSS.Strings.Cursors.Abstract_Cursor'Class)
   is
      use type VSS.Implementation.Strings.Character_Offset;

      Text        : constant not null
        VSS.Implementation.Strings.Variable_Text_Handler_Access :=
          VSS.Implementation.Strings.Variable_Handler (Self.Data);
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

      Text.Compute_Size (From_Cursor, To_Cursor, Size);

      if Size.Index_Offset /= 0 then
         Text.Delete (From_Cursor, Size);

         Self.Notify_String_Modified (From_Cursor, Size, (0, 0, 0));
      end if;
   end Delete;

   ------------
   -- Delete --
   ------------

   function Delete
     (Self    : Virtual_String'Class;
      Pattern : VSS.Characters.Virtual_Character) return Virtual_String
   is
      use type VSS.Unicode.Code_Point;

      Source    : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Data);
      Character : VSS.Unicode.Code_Point'Base;
      Position  : aliased VSS.Implementation.Strings.Cursor;
      Offset    : VSS.Implementation.Strings.Cursor_Offset := (0, 0, 0);

   begin
      Source.Before_First_Character (Position);

      return Result : Virtual_String do
         declare
            Target : constant not null
              VSS.Implementation.Strings.Variable_Text_Handler_Access :=
                VSS.Implementation.Strings.Variable_Handler (Result.Data);

         begin
            while Source.Forward_Element (Position, Character) loop
               if Character
                    /= VSS.Characters.Virtual_Character'Pos (Pattern)
               then
                  Target.Append (Character, Offset);
               end if;
            end loop;
         end;
      end return;
   end Delete;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Self   : Virtual_String'Class;
      Suffix : Virtual_String'Class) return Boolean
   is
      use type VSS.Implementation.Strings.Character_Count;

      Text        : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Data);
      Suffix_Text : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Suffix.Data);

   begin
      if Text.Length < Suffix_Text.Length then
         return False;

      else
         return Text.Ends_With (Suffix_Text.all);
      end if;
   end Ends_With;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Self   : Virtual_String'Class;
      Suffix : VSS.Characters.Virtual_Character) return Boolean
   is
      Text   : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Data);
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      if Text.Is_Empty then
         return False;

      else
         declare
            Aux      : VSS.Implementation.Strings.String_Data;
            Aux_Text :
              VSS.Implementation.Strings.Variable_Text_Handler_Access;

         begin
            Aux_Text := VSS.Implementation.Strings.Variable_Handler (Aux);
            Aux_Text.Append
              (VSS.Characters.Virtual_Character'Pos (Suffix), Offset);

            return
              Text.Ends_With
                (VSS.Implementation.Strings.Constant_Handler (Aux).all);
         end;
      end if;
   end Ends_With;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Self : Virtual_String'Class; Suffix : Line_Terminator) return Boolean is
   begin
      return
        Self.Ends_With (VSS.Implementation.Line_Terminator.Sequence (Suffix));
   end Ends_With;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Self   : Virtual_String'Class;
      Suffix : Line_Terminator_Set) return Boolean is
   begin
      for J in Suffix'Range loop
         if Suffix (J) and then Self.Ends_With (J) then
            return True;
         end if;
      end loop;

      return False;
   end Ends_With;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String) is
   begin
      --  Invalidate and disconnect all referals

      VSS.Implementation.Referrers.Magic_String_Base (Self).Finalize;

      --  Unreference shared data

      VSS.Implementation.Strings.Unreference (Self.Data);
   end Finalize;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Virtual_String'Class) return Hash_Type is
      Generator : VSS.Implementation.FNV_Hash.FNV_1a_Generator;

   begin
      VSS.Implementation.Strings.Constant_Handler (Self.Data).Hash (Generator);

      return
        VSS.Strings.Hash_Type (VSS.Implementation.FNV_Hash.Value (Generator));
   end Hash;

   -----------------
   -- Head_Before --
   -----------------

   function Head_Before
     (Self   : Virtual_String'Class;
      Before : VSS.Strings.Cursors.Abstract_Cursor'Class)
      return Virtual_String
   is
      Handler        : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Data);
      First_Position : aliased VSS.Implementation.Strings.Cursor;
      Last_Position  : VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant
          (Before).all;
      Success        : Boolean with Unreferenced;

   begin
      return Result : Virtual_String do
         if VSS.Strings.Cursors.Internals.Is_Owner (Before, Self) then
            if Handler.Backward (Last_Position) then
               Handler.Before_First_Character (First_Position);
               Success := Handler.Forward (First_Position);
               Handler.Slice (First_Position, Last_Position, Result.Data);
            end if;
         end if;
      end return;
   end Head_Before;

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

      VSS.Implementation.Strings.Variable_Handler (Self.Data).Insert
        (Start, VSS.Characters.Virtual_Character'Pos (Item), Offset);

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

      VSS.Implementation.Strings.Variable_Handler (Self.Data).Insert
        (Start, Item.Data, Offset);

      Self.Notify_String_Modified (Start, (0, 0, 0), Offset);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Virtual_String'Class) return Boolean is
   begin
      return VSS.Implementation.Strings.Constant_Handler (Self.Data).Is_Empty;
   end Is_Empty;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Virtual_String'Class) return Boolean is
   begin
      return VSS.Implementation.Strings.Constant_Handler (Self.Data).Is_Null;
   end Is_Null;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self : in out Virtual_String'Class;
      Item : VSS.Characters.Virtual_Character)
   is
      Offset : VSS.Implementation.Strings.Cursor_Offset;

   begin
      VSS.Implementation.Strings.Variable_Handler (Self.Data).Insert
        ((Index => 1, UTF8_Offset => 0, UTF16_Offset => 0),
         VSS.Characters.Virtual_Character'Pos (Item),
         Offset);

      Self.Notify_String_Modified
        ((Index => 1, UTF8_Offset => 0, UTF16_Offset => 0),
         (Index_Offset => 0, UTF8_Offset => 0, UTF16_Offset => 0),
         Offset);
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
      VSS.Implementation.Strings.Variable_Handler (Self.Data).Insert
        ((Index => 1, UTF8_Offset => 0, UTF16_Offset => 0),
         Item.Data,
         Offset);

      Self.Notify_String_Modified
        ((Index => 1, UTF8_Offset => 0, UTF16_Offset => 0),
         (Index_Offset => 0, UTF8_Offset => 0, UTF16_Offset => 0),
         Offset);
   end Prepend;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Virtual_String)
   is
      Aux      : Wide_Wide_String (1 .. 1);
      Handler  : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Item.Data);
      Position : aliased VSS.Implementation.Strings.Cursor;

   begin
      Buffer.Wide_Wide_Put ("""");

      Handler.Before_First_Character (Position);

      while Handler.Forward (Position) loop
         Aux (1) := Wide_Wide_Character'Val (Handler.Element (Position));

         if Aux (1) =
           Wide_Wide_Character'Val
             (VSS.Implementation.Character_Codes.Quotation_Mark)
         then
            Buffer.Wide_Wide_Put ("""");
         end if;

         Buffer.Wide_Wide_Put (Aux);
      end loop;

      Buffer.Wide_Wide_Put ("""");
   end Put_Image;

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

      VSS.Implementation.Strings.Variable_Handler (Self.Data).Compute_Size
        (From_Cursor, To_Cursor, Deleted);

      if Deleted.Index_Offset /= 0 then
         VSS.Implementation.Strings.Variable_Handler (Self.Data).Delete
           (From_Cursor, Deleted);
      end if;

      VSS.Implementation.Strings.Variable_Handler (Self.Data).Insert
        (From_Cursor, VSS.Characters.Virtual_Character'Pos (By), Inserted);

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
      Text        :
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
          renames VSS.Implementation.Strings.Variable_Handler (Self.Data).all;

   begin
      if not VSS.Strings.Cursors.Internals.Is_Owner (From, Self)
        or else not VSS.Strings.Cursors.Internals.Is_Owner (To, Self)
      then
         return;
      end if;

      Text.Compute_Size (From_Cursor, To_Cursor, Deleted);

      if Deleted.Index_Offset /= 0 then
         Text.Delete (From_Cursor, Deleted);
      end if;

      Text.Insert (From_Cursor, By.Data, Inserted);

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
            VSS.Implementation.Strings.Constant_Handler (Self.Data).Slice
              (First_Position.all, Last_Position.all, Result.Data);
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
            VSS.Implementation.Strings.Constant_Handler (Self.Data).Slice
              (First_Position.all,
               Last_Position.all,
               Result.Data);
         end if;
      end return;
   end Slice;

   -----------
   -- Split --
   -----------

   function Split
     (Self                : Virtual_String'Class;
      Separator           : VSS.Characters.Virtual_Character;
      Keep_Empty_Segments : Boolean := True)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         VSS.Implementation.Strings.Constant_Handler (Self.Data).Split
           (Self.Data,
            VSS.Characters.Virtual_Character'Pos (Separator),
            Keep_Empty_Segments,
            VSS.String_Vectors.Internals.Data_Access (Result).all);
      end return;
   end Split;

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
         VSS.Implementation.Strings.Constant_Handler (Self.Data).Split_Lines
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
     (Self   : Virtual_String'Class;
      Prefix : Virtual_String'Class) return Boolean
   is
      use type VSS.Implementation.Strings.Character_Count;

      Text        : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Data);
      Prefix_Text : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Prefix.Data);

   begin
      if Text.Length < Prefix_Text.Length then
         return False;

      else
         return Text.Starts_With (Prefix_Text.all);
      end if;
   end Starts_With;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With
     (Self        : Virtual_String'Class;
      Prefix      : Virtual_String'Class;
      Transformer : VSS.Transformers.Abstract_Transformer'Class)
      return Boolean
   is
      Self_Transformed   : constant VSS.Strings.Virtual_String :=
         Transformer.Transform (Self);
      Prefix_Transformed : constant VSS.Strings.Virtual_String :=
         Transformer.Transform (Prefix);

   begin
      return Self_Transformed.Starts_With (Prefix_Transformed);
   end Starts_With;

   ----------------
   -- Tail_After --
   ----------------

   function Tail_After
     (Self  : Virtual_String'Class;
      After : VSS.Strings.Cursors.Abstract_Cursor'Class) return Virtual_String
   is
      Text           : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Data);
      First_Position : aliased VSS.Implementation.Strings.Cursor :=
        VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant (After).all;
      Last_Position  : VSS.Implementation.Strings.Cursor;
      Success        : Boolean with Unreferenced;

   begin
      return Result : Virtual_String do
         if VSS.Strings.Cursors.Internals.Is_Owner (After, Self) then
            if Text.Forward (First_Position) then
               Text.After_Last_Character (Last_Position);
               Success := Text.Backward (Last_Position);
               Text.Slice (First_Position, Last_Position, Result.Data);
            end if;
         end if;
      end return;
   end Tail_After;

   ---------------
   -- Tail_From --
   ---------------

   function Tail_From
     (Self : Virtual_String'Class;
      From : VSS.Strings.Cursors.Abstract_Cursor'Class) return Virtual_String
   is
      Text          : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Data);
      From_Position :
        constant VSS.Strings.Cursors.Internals.Cursor_Constant_Access :=
          VSS.Strings.Cursors.Internals.First_Cursor_Access_Constant (From);
      Last_Position : VSS.Implementation.Strings.Cursor;
      Success       : Boolean with Unreferenced;

   begin
      return Result : Virtual_String do
         if VSS.Strings.Cursors.Internals.Is_Owner (From, Self) then
            Text.After_Last_Character (Last_Position);
            Success := Text.Backward (Last_Position);
            Text.Slice (From_Position.all, Last_Position, Result.Data);
         end if;
      end return;
   end Tail_From;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Wide_Wide_String) return Virtual_String
   is
      Success : Boolean;

   begin
      return Result : Virtual_String do
         declare
            Text : constant not null
              VSS.Implementation.Strings.Variable_Text_Handler_Access :=
                VSS.Implementation.Strings.Variable_Handler (Result.Data);

         begin
            Text.From_Wide_Wide_String (Item, Success);

            if not Success then
               raise Constraint_Error with "Ill-formed UTF-32 data";
            end if;
         end;
      end return;
   end To_Virtual_String;

   ------------------------------
   -- To_Virtual_String_Vector --
   ------------------------------

   function To_Virtual_String_Vector
     (Self : Virtual_String'Class)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         Result.Append (Self);
      end return;
   end To_Virtual_String_Vector;

   ---------------
   -- Transform --
   ---------------

   function Transform
     (Self        : Virtual_String'Class;
      Transformer : VSS.Transformers.Abstract_Transformer'Class)
      return Virtual_String is
   begin
      return Transformer.Transform (Self);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Self        : in out Virtual_String'Class;
      Transformer : VSS.Transformers.Abstract_Transformer'Class) is
   begin
      Transformer.Transform (Self);
   end Transform;

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
