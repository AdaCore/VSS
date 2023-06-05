--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Binding of built-in functions provided by GCC. Some of them might require
--  specific '-m<>' switches to compile compilation unit that use this package.

with Interfaces;

package VSS.Implementation.GCC is

   pragma Preelaborate;

   function bswap32 (X : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     with Import,
          Convention    => Intrinsic,
          External_Name => "__builtin_bswap32";

   function clz (X : Interfaces.Unsigned_32) return Interfaces.Integer_32
     with Import,
          Convention    => Intrinsic,
          External_Name => "__builtin_clz";

   --  X86: requires -mbmi2

   function pext_u32
     (X : Interfaces.Unsigned_32;
      Y : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     with Import,
          Convention    => Intrinsic,
          External_Name => "__builtin_ia32_pext_si";

end VSS.Implementation.GCC;
