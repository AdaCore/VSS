------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

package body Magic_Strings.Counted is

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

end Magic_Strings.Counted;
