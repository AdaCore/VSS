------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

package body VSS.Implementation.String_Vectors is

   procedure Free is
     new Ada.Unchecked_Deallocation
       (VSS.Implementation.String_Vectors.String_Vector_Data,
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   Growth_Factor : constant := 2;
   --  The growth factor controls how much extra space is allocated when
   --  we have to increase the size of an allocated vector storage. By
   --  allocating extra space, we avoid the need to reallocate on every
   --  append, particularly important when a vector is built up by repeated
   --  append operations of an individual items. This is expressed as a
   --  factor so 2 means add 1/2 of the length of the vector as growth space.

   procedure Mutate
     (Self     : in out String_Vector_Data_Access;
      Required : Natural;
      Reserved : Natural);
   --  Prepare object to be modified and reserve space for at least Required
   --  number of items and up to additional Reserved number of items.
   --  Parameters Required and Reserve are separated to prevent potential
   --  integer overflow at the caller side.

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) is
   begin
      if Self = null then
         Mutate (Self, 1, 0);

      else
         Mutate (Self, Self.Last + 1, Self.Last / Growth_Factor);
      end if;

      Self.Last := Self.Last + 1;
      Self.Data (Self.Last) := Item;

      VSS.Implementation.Strings.Reference (Self.Data (Self.Last));
   end Append;

   -------------------------------
   -- Append_And_Move_Ownership --
   -------------------------------

   procedure Append_And_Move_Ownership
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) is
   begin
      if Self = null then
         Mutate (Self, 1, 0);

      else
         Mutate (Self, Self.Last + 1, Self.Last / Growth_Factor);
      end if;

      Self.Last := Self.Last + 1;
      Self.Data (Self.Last) := Item;
   end Append_And_Move_Ownership;

   ------------
   -- Mutate --
   ------------

   procedure Mutate
     (Self     : in out String_Vector_Data_Access;
      Required : Natural;
      Reserved : Natural)
   is
   begin
      if Self = null then
         Self := new String_Vector_Data (Required + Reserved);

      elsif not System.Atomic_Counters.Is_One (Self.Counter)
        or else Self.Bulk < Required
      then
         declare
            Old : String_Vector_Data_Access := Self;

         begin
            Self := new String_Vector_Data (Required + Reserved);
            Self.Last := Old.Last;
            Self.Data (1 .. Old.Last) := Old.Data (1 .. Old.Last);

            if not System.Atomic_Counters.Is_One (Old.Counter) then
               for Data of Self.Data (1 .. Self.Last) loop
                  VSS.Implementation.Strings.Reference (Data);
               end loop;

               Unreference (Old);

            else
               Free (Old);
            end if;
         end;
      end if;
   end Mutate;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : String_Vector_Data_Access) is
   begin
      if Self /= null then
         System.Atomic_Counters.Increment (Self.Counter);
      end if;
   end Reference;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self  : in out not null String_Vector_Data_Access;
      Index : Positive;
      Item  : VSS.Implementation.Strings.String_Data) is
   begin
      Mutate (Self, Self.Last, 0);
      VSS.Implementation.Strings.Unreference (Self.Data (Index));
      Self.Data (Index) := Item;
      VSS.Implementation.Strings.Reference (Self.Data (Index));
   end Replace;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out String_Vector_Data_Access) is
   begin
      if Self /= null
        and then System.Atomic_Counters.Decrement (Self.Counter)
      then
         for J in 1 .. Self.Last loop
            VSS.Implementation.Strings.Unreference (Self.Data (J));
         end loop;

         Free (Self);
      end if;
   end Unreference;

end VSS.Implementation.String_Vectors;
