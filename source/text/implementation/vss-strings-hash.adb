--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

function VSS.Strings.Hash
  (Self : VSS.Strings.Virtual_String)
    return Ada.Containers.Hash_Type is
begin
   return Ada.Containers.Hash_Type'Mod (VSS.Strings.Hash_Type'(Self.Hash));
end VSS.Strings.Hash;
