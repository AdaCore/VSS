--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Regular_Expressions.Utilities is

   ------------
   -- Escape --
   ------------

   function Escape
     (Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Item);
   begin
      return raise Program_Error with "Unimplemented Escape";
   end Escape;

   ------------------------------------
   -- Wildcard_To_Regular_Expression --
   ------------------------------------

   function Wildcard_To_Regular_Expression
     (Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Item);
   begin
      return raise Program_Error
        with "Unimplemented Wildcard_To_Regular_Expression";
   end Wildcard_To_Regular_Expression;

end VSS.Regular_Expressions.Utilities;
