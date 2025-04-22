--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

function VSS.IRIs.Hash (Self : VSS.IRIs.IRI) return Ada.Containers.Hash_Type is
begin
   return Ada.Containers.Hash_Type'Mod (VSS.Strings.Hash_Type'(Self.Hash));
end VSS.IRIs.Hash;
