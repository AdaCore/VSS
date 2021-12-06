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

package body Ada.Strings.Wide_Wide_Unbounded.VSS_Aux is

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (U      : out Unbounded_Wide_Wide_String;
      Length : Positive;
      Set    : not null access procedure (S : out Wide_Wide_String))
   is
      TR : constant Shared_Wide_Wide_String_Access := U.Reference;
      DR : Shared_Wide_Wide_String_Access;
   begin
      --  Try to reuse existing shared string

      if Can_Be_Reused (TR, Length) then
         Reference (TR);
         DR := TR;

      --  Otherwise allocate new shared string

      else
         DR := Allocate (Length);
         U.Reference := DR;
      end if;

      Set (DR.Data (1 .. Length));
      DR.Last := Length;
      Unreference (TR);
   end Set_String;

end Ada.Strings.Wide_Wide_Unbounded.VSS_Aux;
