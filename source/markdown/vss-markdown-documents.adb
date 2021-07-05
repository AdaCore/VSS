------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
with System.Atomic_Counters;

with VSS.Markdown.Blocks.Internals;

package body VSS.Markdown.Documents is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Document) is
   begin
      if Self.Data.Assigned then
         System.Atomic_Counters.Increment (Self.Data.Counter);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Document) is
      procedure Free is new Ada.Unchecked_Deallocation
        (VSS.Implementation.Markdown.Abstract_Container_Block'Class,
         Abstract_Container_Block_Access);
   begin
      if not Self.Data.Assigned then
         null;
      elsif System.Atomic_Counters.Decrement (Self.Data.Counter) then

         for Item of Self.Data.Children loop
            if System.Atomic_Counters.Decrement (Item.Counter) then
               VSS.Implementation.Markdown.Free (Item);
            end if;
         end loop;

         Free (Self.Data);

      else
         Self.Data := null;
      end if;
   end Finalize;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : Document) return Boolean is
   begin
      return not Self.Data.Assigned or else Self.Data.Children.Is_Empty;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   overriding function Length (Self : Document) return Natural is
   begin
      if Self.Data.Assigned then
         return Self.Data.Children.Last_Index;
      else
         return 0;
      end if;
   end Length;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Document;
      Index : Positive) return VSS.Markdown.Blocks.Block
   is
      Item : constant VSS.Implementation.Markdown.Abstract_Block_Access :=
        Self.Data.Children (Index);
   begin
      System.Atomic_Counters.Increment (Item.Counter);

      return Result : VSS.Markdown.Blocks.Block do
         VSS.Markdown.Blocks.Internals.Set (Result, Item);
      end return;
   end Element;

end VSS.Markdown.Documents;
