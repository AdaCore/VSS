--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

package body VSS.Stream_Element_Vectors is

   use type Ada.Streams.Stream_Element_Count;

   procedure Free is
     new Ada.Unchecked_Deallocation (Data_Record, Data_Access);

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : Stream_Element_Vector;
      Right : Stream_Element_Vector) return Boolean
   is
      use type Ada.Streams.Stream_Element_Array;

   begin
      return
        Left.Data = Right.Data
        or else (Left.Data /= null and then Right.Data /= null
                 and then Left.Data.Length = Right.Data.Length
                 and then Left.Data.Storage (1 .. Left.Data.Length)
                   = Right.Data.Storage (1 .. Right.Data.Length));
   end "=";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Stream_Element_Vector) is
      Source : constant Data_Access := Self.Data;

   begin
      if Source /= null then
         Self.Data :=
           new Data_Record
             (Ada.Streams.Stream_Element_Offset'Max
                (Source.Size, Self.Capacity));

         Self.Data.Length := Source.Length;
         Self.Data.Storage (Source.Storage'Range) := Source.Storage;
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Stream_Element_Vector'Class;
      Item : Ada.Streams.Stream_Element)
   is
      Source : Data_Access := Self.Data;

   begin
      if Self.Data = null then
         Self.Data := new Data_Record (1);
         Self.Data.Length := 0;

      elsif Self.Data.Size <= Self.Data.Length then
         Self.Data :=
           new Data_Record
             (Ada.Streams.Stream_Element_Offset'Max
                (Source.Size * 2, Self.Capacity));
         Self.Data.Length := Source.Length;
         Self.Data.Storage (Source.Storage'Range) := Source.Storage;
         Free (Source);
      end if;

      Self.Data.Length := Self.Data.Length + 1;
      Self.Data.Storage (Self.Data.Length) := Item;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Stream_Element_Vector'Class;
      Item : Stream_Element_Vector'Class)
   is
      Prefix : Data_Access := Self.Data;
      Suffix : constant Data_Access := Item.Data;

   begin
      if Suffix = null or else Suffix.Length = 0 then
         return;
      end if;

      if Prefix = null then
         Self.Data :=
           new Data_Record
             (Ada.Streams.Stream_Element_Offset'Max
                (Suffix.Size, Self.Capacity));
         Self.Data.Length := Suffix.Length;
         Self.Data.Storage (Suffix.Storage'Range) := Suffix.Storage;

      else
         Self.Data :=
           new Data_Record
                 (Ada.Streams.Stream_Element_Offset'Max
                    (Prefix.Length + Item.Length, Self.Capacity));
         Self.Data.Length := Prefix.Length + Item.Length;
         Self.Data.Storage (1 .. Prefix.Length) :=
           Prefix.Storage (1 .. Prefix.Length);
         Self.Data.Storage (Prefix.Length + 1 .. Self.Data.Length) :=
           Item.Data.Storage (1 .. Item.Data.Length);
         Free (Prefix);
      end if;
   end Append;

   -------------
   -- Element --
   -------------

   function Element
     (Self  : Stream_Element_Vector'Class;
      Index : Ada.Streams.Stream_Element_Count)
      return Ada.Streams.Stream_Element is
   begin
      if Self.Data = null or else Self.Data.Length < Index then
         raise Constraint_Error;
      end if;

      return Self.Data.Storage (Index);
   end Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self     : Stream_Element_Vector'Class;
      Position : Cursor) return Ada.Streams.Stream_Element is
   begin
      return Self.Element (Position.Index);
   end Element;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Stream_Element_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   function Has_Element (Self : Cursor) return Boolean is (Self.Index > 0);

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Reversible_Iterator) return Cursor is
   begin
      return (Index => (if Self.Last > 0 then 1 else 0));
   end First;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Stream_Element_Vector'Class) return Reversible_Iterator is
   begin
      return (Last => Self.Length);
   end Iterate;

   ----------
   -- Last --
   ----------

   overriding function Last (Self : Reversible_Iterator) return Cursor is
   begin
      return (Index => Self.Last);
   end Last;

   ------------
   -- Length --
   ------------

   function Length
     (Self : Stream_Element_Vector'Class)
      return Ada.Streams.Stream_Element_Count is
   begin
      return (if Self.Data = null then 0 else Self.Data.Length);
   end Length;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Reversible_Iterator;
      Position : Cursor) return Cursor
   is
      Index : constant Ada.Streams.Stream_Element_Count :=
        (if Position.Index < Self.Last then Position.Index + 1 else 0);
   begin
      return (Index => Index);
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Self     : Reversible_Iterator;
      Position : Cursor) return Cursor
   is
      pragma Unreferenced (Self);
   begin
      return (Index => (if Position.Index > 0 then Position.Index - 1 else 0));
   end Previous;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Stream_Element_Vector) is
   begin
      null;
   end Read;

   ------------------
   -- Set_Capacity --
   ------------------

   procedure Set_Capacity
     (Self     : in out Stream_Element_Vector'Class;
      Capacity : Ada.Streams.Stream_Element_Count) is
   begin
      Self.Capacity := Capacity;
   end Set_Capacity;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Stream_Element_Vector) is
   begin
      if Item.Data /= null then
         Ada.Streams.Stream_Element_Array'Write
           (Stream, Item.Data.Storage (1 .. Item.Data.Length));
      end if;
   end Write;

end VSS.Stream_Element_Vectors;
