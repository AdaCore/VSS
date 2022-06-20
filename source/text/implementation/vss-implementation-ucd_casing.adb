--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
