--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
