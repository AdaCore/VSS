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

with Ada.Unchecked_Deallocation;

package body VSS.Stream_Element_Buffers is

   use type Ada.Streams.Stream_Element_Offset;

   procedure Free is
     new Ada.Unchecked_Deallocation (Data_Record, Data_Access);

   package Iterators is

      type Forward_Iterator is
        new Stream_Element_Buffer_Iterator_Interfaces.Forward_Iterator
      with record
         Buffer : Stream_Element_Buffer_Access;
      end record;

      overriding function First
        (Self : Forward_Iterator) return Stream_Element_Buffer_Cursor;

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Stream_Element_Buffer_Cursor)
         return Stream_Element_Buffer_Cursor;

   end Iterators;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Stream_Element_Buffer) is
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
     (Self : in out Stream_Element_Buffer;
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

   -------------------------
   -- Each_Stream_Element --
   -------------------------

   function Each_Stream_Element
     (Self : Stream_Element_Buffer'Class)
      return Stream_Element_Buffer_Iterator_Interfaces.Forward_Iterator'Class
   is
   begin
      return
        Iterators.Forward_Iterator'(Buffer => Self'Unrestricted_Access);
   end Each_Stream_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self  : Stream_Element_Buffer'Class;
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
     (Self : Stream_Element_Buffer_Cursor'Class)
      return Ada.Streams.Stream_Element
   is
   begin
      if Self.Has_Element then
         return Self.Buffer.Data.Storage (Self.Index);

      else
         raise Constraint_Error;
      end if;
   end Element;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Stream_Element_Buffer) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Stream_Element_Buffer_Cursor) return Boolean is
   begin
      return
        Self.Buffer /= null
          and then Self.Buffer.Data /= null
          and then Self.Index > 0
          and then Self.Buffer.Data.Length <= Self.Index;
   end Has_Element;

   ---------------
   -- Iterators --
   ---------------

   package body Iterators is

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Forward_Iterator) return Stream_Element_Buffer_Cursor is
      begin
         return (Buffer => Self.Buffer, Index => 1);
      end First;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Stream_Element_Buffer_Cursor)
         return Stream_Element_Buffer_Cursor is
      begin
         return (Buffer => Position.Buffer, Index => Position.Index + 1);
      end Next;

   end Iterators;

   ------------
   -- Length --
   ------------

   function Length
     (Self : Stream_Element_Buffer'Class)
      return Ada.Streams.Stream_Element_Count is
   begin
      return (if Self.Data = null then 0 else Self.Data.Length);
   end Length;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Stream_Element_Buffer) is
   begin
      null;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Stream_Element_Buffer_Cursor) is
   begin
      raise Program_Error;
   end Read;

   ------------------
   -- Set_Capacity --
   ------------------

   procedure Set_Capacity
     (Self     : in out Stream_Element_Buffer'Class;
      Capacity : Ada.Streams.Stream_Element_Count) is
   begin
      Self.Capacity := Capacity;
   end Set_Capacity;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Stream_Element_Buffer) is
   begin
      if Item.Data /= null then
         Ada.Streams.Stream_Element_Array'Write
           (Stream, Item.Data.Storage (1 .. Item.Data.Length));
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Stream_Element_Buffer_Cursor) is
   begin
      raise Program_Error;
   end Write;

end VSS.Stream_Element_Buffers;
