--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
