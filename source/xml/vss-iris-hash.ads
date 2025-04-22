--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;

function VSS.IRIs.Hash (Self : VSS.IRIs.IRI) return Ada.Containers.Hash_Type
  with Preelaborate;
