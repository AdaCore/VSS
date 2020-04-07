------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Magic.Stream_Element_Buffers is

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
      use type Ada.Streams.Stream_Element_Offset;

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
      end if;

      Self.Data.Length := Self.Data.Length + 1;
      Self.Data.Storage (Self.Data.Length) := Item;
   end Append;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Stream_Element_Buffer) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Data_Record, Data_Access);

   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

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

end Magic.Stream_Element_Buffers;
