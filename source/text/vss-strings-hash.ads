--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers;

function VSS.Strings.Hash
  (Self : VSS.Strings.Virtual_String)
   return Ada.Containers.Hash_Type;
pragma Preelaborate (VSS.Strings.Hash);
