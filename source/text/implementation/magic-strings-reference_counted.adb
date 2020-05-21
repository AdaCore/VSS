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

package body Magic.Strings.Reference_Counted is

   type Shared_String_Access is access all Abstract_Shared_String'Class;

   ---------------
   -- Reference --
   ---------------

   overriding function Reference
     (Self : in out Abstract_Shared_String) return String_Access is
   begin
      System.Atomic_Counters.Increment (Self.Counter);

      return Self'Unchecked_Access;
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference (Self : in out Abstract_Shared_String) is
      procedure Free is
        new Ada.Unchecked_Deallocation
              (Abstract_Shared_String'Class, Shared_String_Access);

      Aux : Shared_String_Access := Self'Unchecked_Access;

   begin
      if System.Atomic_Counters.Decrement (Self.Counter) then
         Self.Finalize;
         Free (Aux);
      end if;
   end Unreference;

end Magic.Strings.Reference_Counted;
