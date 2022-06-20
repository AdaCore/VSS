--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces;

with VSS.Strings;

package VSS.JSON is

   pragma Preelaborate;

   type JSON_Number_Kind is (None, JSON_Integer, JSON_Float, Out_Of_Range);
   --  Format of the number used to represent value. Note, it is only hint in
   --  most cases, implementation may use other format for some reason.

   type JSON_Number (Kind : JSON_Number_Kind := None) is record
      case Kind is
         when None =>
            null;

         when others =>
            String_Value : VSS.Strings.Virtual_String;

            case Kind is
               when None | Out_Of_Range =>
                  null;

               when JSON_Integer =>
                  Integer_Value : Interfaces.Integer_64;

               when JSON_Float =>
                  Float_Value   : Interfaces.IEEE_Float_64;
            end case;
      end case;
   end record;

   function As_Integer (Self : JSON_Number) return Interfaces.Integer_64;
   --  Return number value as integer number. Non-integer value is rounded to
   --  integer.
   --  @exception Constraint_Error is raised when value is out of range or not
   --  present.

   function As_Float (Self : JSON_Number) return Interfaces.IEEE_Float_64;
   --  Return number value as floating point value.
   --  @exception Constraint_Error is raised when value is out of range or not
   --  present.

end VSS.JSON;
