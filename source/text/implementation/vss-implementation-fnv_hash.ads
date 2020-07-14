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
--  This package provides hash generator for FNV-1a algoriphm.

with System.Storage_Elements;

package VSS.Implementation.FNV_Hash is

   pragma Preelaborate;

   type Hash_64_Type is mod 2 ** 64;

   type FNV_1a_Generator is limited private;

   procedure Hash
     (Self : in out FNV_1a_Generator;
      Data : System.Storage_Elements.Storage_Element) with Inline_Always;
   --  Continue hash computation by adding given byte.

   function Value (Self : FNV_1a_Generator) return Hash_64_Type;
   --  Return current computed hash value.

private

   Offset_Basis_64 : constant Hash_64_Type := 16#CBF29CE4_84222325#;
   FNV_Prime_64    : constant Hash_64_Type := 16#00000100_000001B3#;

   type FNV_1a_Generator is limited record
      Value : Hash_64_Type := Offset_Basis_64;
   end record;

end VSS.Implementation.FNV_Hash;
