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

with Ada.Unchecked_Conversion;

package body VSS.Implementation.UCD_Casing is

   type Unsigned_6 is mod 2**6 with Size => 6;

   function To_Unsigned_6 is
     new Ada.Unchecked_Conversion (Casing_Context, Unsigned_6);

   function To_Unsigned_6 is
     new Ada.Unchecked_Conversion (Casing_Context_Change, Unsigned_6);

   function To_Casing_Context is
     new Ada.Unchecked_Conversion (Unsigned_6, Casing_Context);

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Context : in out Casing_Context;
      Change  : Casing_Context_Change) is
   begin
      Context :=
        To_Casing_Context
          (To_Unsigned_6 (Change)
           or (To_Unsigned_6 (Context) and (To_Unsigned_6 (Change) / 2)));
   end Apply;

end VSS.Implementation.UCD_Casing;
