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

with VSS.Implementation.String_Handlers;

package body VSS.Implementation.String_Vectors is

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : String_Vector_Data_Access) is
   begin
      if Self /= null then
         System.Atomic_Counters.Increment (Self.Counter);
      end if;
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out String_Vector_Data_Access) is

      procedure Free is
        new Ada.Unchecked_Deallocation
          (VSS.Implementation.String_Vectors.String_Vector_Data,
           VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   begin
      if Self /= null
        and then System.Atomic_Counters.Decrement (Self.Counter)
      then
         for J in 1 .. Self.Last loop
            declare
               Handler : constant access
                 VSS.Implementation.String_Handlers
                   .Abstract_String_Handler'Class
                     := VSS.Implementation.Strings.Handler (Self.Data (J));

            begin
               Handler.Unreference (Self.Data (J));
            end;
         end loop;

         Free (Self);
      end if;
   end Unreference;

end VSS.Implementation.String_Vectors;
