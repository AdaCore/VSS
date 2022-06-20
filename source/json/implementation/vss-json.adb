--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.JSON is

   --------------
   -- As_Float --
   --------------

   function As_Float (Self : JSON_Number) return Interfaces.IEEE_Float_64 is
   begin
      case Self.Kind is
         when None | Out_Of_Range =>
            raise Constraint_Error;

         when JSON_Float =>
            return Self.Float_Value;

         when JSON_Integer =>
            return Interfaces.IEEE_Float_64 (Self.Integer_Value);
      end case;
   end As_Float;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : JSON_Number) return Interfaces.Integer_64 is
   begin
      case Self.Kind is
         when None | Out_Of_Range =>
            raise Constraint_Error;

         when JSON_Float =>
            return Interfaces.Integer_64 (Self.Float_Value);

         when JSON_Integer =>
            return Self.Integer_Value;
      end case;
   end As_Integer;

end VSS.JSON;
