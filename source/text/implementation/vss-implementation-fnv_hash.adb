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

package body VSS.Implementation.FNV_Hash is

   ----------
   -- Hash --
   ----------

   procedure Hash
     (Self : in out FNV_1a_Generator;
      Data : System.Storage_Elements.Storage_Element) is
   begin
      Self.Value := Self.Value xor Hash_64_Type (Data);
      Self.Value := Self.Value * FNV_Prime_64;
   end Hash;

   -----------
   -- Value --
   -----------

   function Value (Self : FNV_1a_Generator) return Hash_64_Type is
   begin
      return Self.Value;
   end Value;

end VSS.Implementation.FNV_Hash;
