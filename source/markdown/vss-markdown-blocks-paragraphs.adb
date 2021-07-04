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

with System.Atomic_Counters;

package body VSS.Markdown.Blocks.Paragraphs is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Paragraph) is
   begin
      if Self.Data.Assigned then
         System.Atomic_Counters.Increment (Self.Data.Counter);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Paragraph) is
   begin
      if Self.Data.Assigned then
         if System.Atomic_Counters.Decrement (Self.Data.Counter) then
            VSS.Implementation.Markdown.Free
              (VSS.Implementation.Markdown.Abstract_Block_Access (Self.Data));

         else
            Self.Data := null;
         end if;
      end if;
   end Finalize;

   ----------------
   -- From_Block --
   ----------------

   function From_Block (Self : VSS.Markdown.Blocks.Block) return Paragraph is
   begin
      System.Atomic_Counters.Increment (Self.Data.Counter);

      return (Ada.Finalization.Controlled with Data =>
               Paragraph_Access (Self.Data));
   end From_Block;

   ----------
   -- Text --
   ----------

   function Text
     (Self : Paragraph) return VSS.Markdown.Annotations.Annotated_Text is
   begin
      return Self.Data.Text;
   end Text;

   --------------
   -- To_Block --
   --------------

   function To_Block (Self : Paragraph) return VSS.Markdown.Blocks.Block is
   begin
      return (Ada.Finalization.Controlled with Data =>
               VSS.Implementation.Markdown.Abstract_Block_Access (Self.Data));
   end To_Block;

end VSS.Markdown.Blocks.Paragraphs;
